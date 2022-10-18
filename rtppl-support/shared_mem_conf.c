/**
 * @file shared_mem_conf.c
 * @author Matthias Becker
 * @brief Wrapper for shared memory communication.
 * @date 2022-09-27
 *
 * @copyright Copyright (c) 2022
 *
 */

/*******************************************************************************
  THE CONTENT OF THIS FILE SHOULD BE GENERATED.
*******************************************************************************/

#include "shared_mem.h"
#include "shared_mem_conf.h"

/**
 * All signals shared on the node.
 */
shared_mem_t sm_regs[] = {
  {
    .name = "DistanceFrontLeft",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "DistanceFrontRight",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "DistanceBackLeft",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "DistanceBackRight",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "DistanceSideLeft",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "DistanceSideRight",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "SpeedWheelLeft",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "SpeedWheelRight",
    .size = 1 << 16,
    .mgmt = NULL
  },{
    .name = "ObservedDistanceFront",
    .size = 1 << 16,
    .mgmt = NULL
  }
};
