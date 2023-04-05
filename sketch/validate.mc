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
  | UnusedTaskInput Info
  | UnusedTaskOutput Info
  | UnusedTaskActuatorOutput Info
  | UsedSensorOutput Info
  | UsedActuatorInput Info
  | UsedTaskInput Info
  | UsedTaskOutput Info
  | UsedTaskActuatorOutput Info

  sem portStateInfo : PortState -> Info
  sem portStateInfo =
  | UnusedSensorOutput i -> i
  | UnusedActuatorInput i -> i
  | UnusedTaskInput i -> i
  | UnusedTaskOutput i -> i
  | UnusedTaskActuatorOutput i -> i
  | UsedSensorOutput i -> i
  | UsedActuatorInput i -> i
  | UsedTaskInput i -> i
  | UsedTaskOutput i -> i
  | UsedTaskActuatorOutput i -> i

  -- OPT(larshum, 2023-03-06): Use a more efficient data structure for storing
  -- strings, such as a trie.
  type PortConfig = Map String PortState

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
    let emptyPortConfig = mapEmpty cmpString in
    let functionDecls = foldl addPortToConfig emptyPortConfig declaredPorts in
    {acc with decls = mapInsert id functionDecls acc.decls}
  | _ -> acc

  sem insertPortErr : String -> PortState -> PortConfig -> PortConfig
  sem insertPortErr id s1 =
  | config ->
    match mapLookup id config with Some s2 then
      let psi = portStateInfo in
      errorSingle [psi s1, psi s2] "Port name already in use"
    else mapInsert id s1 config

  sem addPortToConfig : PortConfig -> Port -> PortConfig
  sem addPortToConfig config =
  | InputPort {id = {v = id}, info = i1} ->
    insertPortErr id (UnusedTaskInput i1) config
  | OutputPort {id = {v = id}, info = i1} ->
    insertPortErr id (UnusedTaskOutput i1) config
  | ActuatorOutputPort {id = {v = id}, info = i1} ->
    insertPortErr id (UnusedTaskActuatorOutput i1) config

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
    mapFoldWithKey (insertTaskPort id) network config

  sem insertTaskPort : Name -> Network -> String -> PortState -> Network
  sem insertTaskPort taskId network portId =
  | state ->
    let portId = Internal (taskId, portId) in
    mapInsert portId state network

  sem addConnection : Network -> Connection -> Network
  sem addConnection network =
  | Connection {from = fromSpec, to = toSpec, info = info} ->
    let from = portSpecToPortId fromSpec in
    let to = portSpecToPortId toSpec in
    validateActuatorConnection network info from to;
    let network =
      match mapLookup from network with Some portState then
        mapInsert from (updateOutputPortState info portState) network
      else
        errorSingle [get_PortSpec_info fromSpec]
          "Reference to undefined output port"
    in
    match mapLookup to network with Some portState then
      mapInsert to (updateInputPortState info portState) network
    else
      errorSingle [get_PortSpec_info toSpec]
        "Reference to undefined input port"

  -- NOTE(larshum, 2023-04-05): This function validates that, when the to-port
  -- of a connection is the input port of an actuator, the output port must
  -- have been declared as an actuator output port. This is important as these
  -- have different semantics from the standard output ports.
  sem validateAcutatorConnection : Network -> Info -> PortId -> PortId -> ()
  sem validateActuatorConnection network info from =
  | to ->
    match mapLookup to network
    with Some (UnusedActuatorInput _ | UsedActuatorInput _) then
      match mapLookup from network
      with Some (UnusedTaskActuatorOutput _ | UsedTaskActuatorOutput _) then
        ()
      else
        errorSingle [info]
          "Actuator must be connected to task actuator output ports"
    else ()

  sem portSpecToPortId : PortSpec -> PortId
  sem portSpecToPortId =
  | PortSpec {port = {v = extId}, id = None ()} ->
    External extId
  | PortSpec {port = {v = taskId}, id = Some {v = portId}} ->
    Internal (taskId, portId)

  sem updateOutputPortState : Info -> PortState -> PortState
  sem updateOutputPortState info =
  | UnusedSensorOutput i
  | UsedSensorOutput i ->
    UsedSensorOutput i
  | UnusedTaskOutput i
  | UsedTaskOutput i ->
    UsedTaskOutput i
  | UnusedTaskActuatorOutput i ->
    UsedTaskActuatorOutput i
  | UsedTaskActuatorOutput i ->
    errorSingle [i, info] "Actuator outputs cannot be mapped to multiple input ports"
  | UnusedActuatorInput i
  | UnusedTaskInput i
  | UsedActuatorInput i
  | UsedTaskInput i ->
    errorSingle [i, info] "Input ports cannot be used as output"

  sem updateInputPortState : Info -> PortState -> PortState
  sem updateInputPortState info =
  | UnusedActuatorInput i ->
    UsedActuatorInput i
  | UnusedTaskInput i ->
    UsedTaskInput i
  | UsedActuatorInput i ->
    errorSingle [i, info] "Multiple outputs cannot be mapped to one actuator input"
  | UsedTaskInput i ->
    errorSingle [i, info] "Multiple outputs cannot be mapped to one task input port"
  | UnusedSensorOutput i
  | UnusedTaskOutput i
  | UnusedTaskActuatorOutput i
  | UsedSensorOutput i
  | UsedTaskOutput i
  | UsedTaskActuatorOutput i ->
    errorSingle [i, info] "Output ports cannot be used as input"

  sem validatePortState : () -> PortId -> PortState -> ()
  sem validatePortState acc portId =
  | UnusedActuatorInput i ->
    errorSingle [i] "No task output is connected to this actuator."
  | UnusedSensorOutput i ->
    errorSingle [i] "The output from this sensor is not connected to any task"
  | UnusedTaskOutput i
  | UnusedTaskActuatorOutput i ->
    let msg = join ["The output port ", portIdToString portId, " is unused."] in
    errorSingle [i] msg
  | UnusedTaskInput i ->
    let msg = join ["The input port ", portIdToString portId, " is unused."] in
    errorSingle [i] msg
  | UsedSensorOutput _
  | UsedActuatorInput _
  | UsedTaskInput _
  | UsedTaskOutput _
  | UsedTaskActuatorOutput _ ->
    ()
end

mexpr

use RtpplValidate in

let input = get argv 1 in
let content = readFile input in
let program = parseRtpplExn input content in
validateRtpplProgram program
