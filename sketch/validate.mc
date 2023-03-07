include "./rtppl.mc"

lang RtpplValidate = RtpplAst
  syn PortId =
  | External Name
  | Internal (Name, String)

  sem cmpPortId : PortId -> PortId -> Int
  sem cmpPortId lhs =
  | rhs -> cmpPortIdH (lhs, rhs)

  sem cmpPortIdH : (PortId, PortId) -> Int
  sem cmpPortIdH =
  | (External l, External r) -> nameCmp l r
  | (Internal l, Internal r) ->
    let d = nameCmp l.0 r.0 in
    if neqi d 0 then d else cmpString l.1 r.1
  | (lhs, rhs) -> subi (constructorTag lhs) (constructorTag rhs)

  sem portIdToString : PortId -> String
  sem portIdToString =
  | External id -> nameGetStr id
  | Internal (taskId, portId) ->
    join [nameGetStr taskId, ".", portId]

  syn PortState =
  | UnusedSensorOutput Info
  | UnusedActuatorInput Info
  | UnusedTaskOutput Info
  | UnusedTaskInput Info
  | UsedSensorOutput Info
  | UsedActuatorInput Info
  | UsedTaskOutput Info
  | UsedTaskInput Info

  sem portStateInfo : PortState -> Info
  sem portStateInfo =
  | UnusedSensorOutput i -> i
  | UnusedActuatorInput i -> i
  | UnusedTaskOutput i -> i
  | UnusedTaskInput i -> i
  | UsedSensorOutput i -> i
  | UsedActuatorInput i -> i
  | UsedTaskOutput i -> i
  | UsedTaskInput i -> i

  -- OPT(larshum, 2023-03-06): Use a more efficient data structure for storing
  -- strings, such as a trie.
  type PortConfig = {inputs : Map String Info, outputs : Map String Info}

  type PortDecls = {
    sensors : Map Name Info,
    actuators : Map Name Info,
    decls : Map Name PortConfig
  }

  type Network = Map PortId PortState

  sem validateRtpplProgram : Program -> ()
  sem validateRtpplProgram =
  | Program {tops = tops, main = main} ->
    let emptyPorts = {
      sensors = mapEmpty nameCmp,
      actuators = mapEmpty nameCmp,
      decls = mapEmpty nameCmp
    } in
    let declPorts = foldl collectDeclaredPorts emptyPorts tops in
    validateNetwork declPorts main

  sem collectDeclaredPorts : PortDecls -> Top -> PortDecls
  sem collectDeclaredPorts acc =
  | SensorTop {id = {v = id}, info = info} ->
    {acc with sensors = mapInsert id info acc.sensors}
  | ActuatorTop {id = {v = id}, info = info} ->
    {acc with actuators = mapInsert id info acc.actuators}
  | FunctionDefTop {id = {v = id}, body = {ports = declaredPorts}} ->
    let emptyPortConfig = {
      inputs = mapEmpty cmpString,
      outputs = mapEmpty cmpString
    } in
    let functionDecls = foldl addPortToConfig emptyPortConfig declaredPorts in
    {acc with decls = mapInsert id functionDecls acc.decls}
  | _ -> acc

  sem addPortToConfig : PortConfig -> Port -> PortConfig
  sem addPortToConfig config =
  | InputPort {id = {v = id}, info = i1} ->
    match mapLookup id config.inputs with Some i2 then
      errorSingle [i1, i2] "Duplicate definition of input port"
    else {config with inputs = mapInsert id i1 config.inputs}
  | OutputPort {id = {v = id}, info = i1} ->
    match mapLookup id config.outputs with Some i2 then
      errorSingle [i1, i2] "Duplicate definition of output port"
    else {config with outputs = mapInsert id i1 config.outputs}

  sem validateNetwork : PortDecls -> Main -> ()
  sem validateNetwork declPorts =
  | Main {tasks = tasks, connections = connections} ->
    -- Insert port states for all sensors and actuators.
    let network = mapEmpty cmpPortId in
    let network = mapFoldWithKey insertSensor network declPorts.sensors in
    let network = mapFoldWithKey insertActuator network declPorts.actuators in

    -- Insert ports for each of the tasks based on the declared ports of the
    -- functions they are defined in terms of.
    let network = foldl (insertTaskPorts declPorts.decls) network tasks in

    -- Update the state of the ports based on the connection definitions.
    let network = foldl addConnection network connections in

    -- Validate the resulting network.
    mapFoldWithKey validatePortState () network

  sem insertSensor : Network -> Name -> Info -> Network
  sem insertSensor network id =
  | info ->
    let portId = External id in
    let state = UnusedSensorOutput info in
    mapInsert portId state network

  sem insertActuator : Network -> Name -> Info -> Network
  sem insertActuator network id =
  | info ->
    let portId = External id in
    let state = UnusedActuatorInput info in
    mapInsert portId state network

  sem insertTaskPorts : Map Name PortConfig -> Network -> Task -> Network
  sem insertTaskPorts decls network =
  | Task {id = {v = id}, templateId = {v = tid}, info = info} ->
    let config =
      match mapLookup tid decls with Some config then
        config
      else
        errorSingle [info] "Task is defined in terms of undefined function"
    in
    let network = mapFoldWithKey (insertTaskInputPort id) network config.inputs in
    mapFoldWithKey (insertTaskOutputPort id) network config.outputs

  sem insertTaskInputPort : Name -> Network -> String -> Info -> Network
  sem insertTaskInputPort taskId network portId =
  | info ->
    let portId = Internal (taskId, portId) in
    let state = UnusedTaskInput info in
    mapInsert portId state network

  sem insertTaskOutputPort : Name -> Network -> String -> Info -> Network
  sem insertTaskOutputPort taskId network portId =
  | info ->
    let portId = Internal (taskId, portId) in
    let state = UnusedTaskOutput info in
    mapInsert portId state network

  sem addConnection : Network -> Connection -> Network
  sem addConnection network =
  | Connection {from = fromSpec, to = toSpec, info = info} ->
    let from = channelSpecToPortId fromSpec in
    let network =
      match mapLookup from network with Some portState then
        mapInsert from (updateOutputPortState info portState) network
      else
        errorSingle [get_ChannelSpec_info fromSpec]
          "Reference to undefined output port"
    in
    let to = channelSpecToPortId toSpec in
    match mapLookup to network with Some portState then
      mapInsert to (updateInputPortState info portState) network
    else
      errorSingle [get_ChannelSpec_info toSpec]
        "Reference to undefined input port"

  sem channelSpecToPortId : ChannelSpec -> PortId
  sem channelSpecToPortId =
  | ChannelSpec {port = {v = extId}, id = None ()} ->
    External extId
  | ChannelSpec {port = {v = taskId}, id = Some {v = portId}} ->
    Internal (taskId, portId)

  sem updateOutputPortState : Info -> PortState -> PortState
  sem updateOutputPortState info =
  | UnusedSensorOutput _ ->
    UsedSensorOutput info
  | UnusedTaskOutput _
  | UsedTaskOutput _ ->
    UsedTaskOutput info
  | UsedSensorOutput i ->
    errorSingle [i, info] "Sensor outputs cannot be mapped to multiple input ports"
  | UnusedActuatorInput i
  | UnusedTaskInput i
  | UsedActuatorInput i
  | UsedTaskInput i ->
    errorSingle [i] "Input ports cannot be used as output"

  sem updateInputPortState : Info -> PortState -> PortState
  sem updateInputPortState info =
  | UnusedActuatorInput _ ->
    UsedActuatorInput info
  | UnusedTaskInput _ ->
    UsedTaskInput info
  | UsedActuatorInput i ->
    errorSingle [i, info] "Multiple outputs cannot be mapped to one actuator input"
  | UsedTaskInput i ->
    errorSingle [i, info] "Multiple outputs cannot be mapped to one task input port"
  | UnusedSensorOutput i
  | UnusedTaskOutput i
  | UsedSensorOutput i
  | UsedTaskOutput i ->
    errorSingle [i] "Output ports cannot be used as input"

  sem validatePortState : () -> PortId -> PortState -> ()
  sem validatePortState acc portId =
  | UnusedSensorOutput i ->
    errorSingle [i] "The output from this sensor is not connected to any task"
  | UnusedActuatorInput i ->
    errorSingle [i] "No task output is connected to this actuator."
  | UnusedTaskOutput i ->
    let msg = join ["The output port ", portIdToString portId, " is unused."] in
    errorSingle [i] msg
  | UnusedTaskInput i ->
    let msg = join ["The input port ", portIdToString portId, " is unused."] in
    errorSingle [i] msg
  | UsedSensorOutput _
  | UsedActuatorInput _
  | UsedTaskOutput _
  | UsedTaskInput _ -> ()
end

mexpr

use RtpplValidate in

let input = get argv 1 in
let content = readFile input in
let program = parseRtpplExn input content in
validateRtpplProgram program
