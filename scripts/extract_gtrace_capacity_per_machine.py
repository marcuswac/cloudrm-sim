#!/usr/bin/python

from __future__ import print_function, division
import sys
from collections import namedtuple
from math import ceil

MachineEvent = namedtuple('MachineEvent', ['time', 'machineId', 'type',
                                           'platform', 'cpu', 'mem'])

TIME_WINDOW_SIZE = 300000000 
MACHINE_EVENTS_FILE = "part-00000-of-00001.csv"

class MachineInfo:
    def __init__(self, cpu, mem):
        try:
            self.cpu = float(cpu)
        except:
            self.cpu = 0
        try:
            self.mem = float(mem)
        except:
            self.mem = 0

def extractMachinesInfo(f, windowSize):
    machines = {}
    currentWindow = 0
    currentCpuCapacity = 0
    currentMemCapacity = 0

    for line in f:
        token = line.split(',')
        event = MachineEvent._make(token)
        timeWindow = int(ceil(int(event.time) / windowSize) * windowSize)
        
        while currentWindow < timeWindow:
            print(currentWindow, currentCpuCapacity, currentMemCapacity)
            currentWindow += windowSize 

        if event.type == '0': # added 
            machine = MachineInfo(event.cpu, event.mem)
            machines[event.machineId] = machine 
            currentCpuCapacity += machine.cpu
            currentMemCapacity += machine.mem
        elif event.type == '1': # removed
            machine = machines[event.machineId]
            currentCpuCapacity -= machine.cpu
            currentMemCapacity -= machine.mem
            del machines[event.machineId]
        elif event.type == '2': # updated
            machine = machines[event.machineId]
            newInfo = MachineInfo(event.cpu, event.mem)
            currentCpuCapacity += newInfo.cpu - machine.cpu
            currentMemCapacity += newInfo.mem - machine.mem
            machines[event.machineId] = newInfo 
        else: # invalid type 
            raise

def main(argv):
    machineEventsFile = argv[0] if len(argv) > 0 else MACHINE_EVENTS_FILE
    timeWindowSize = int(argv[1]) if len(argv) > 1 else TIME_WINDOW_SIZE
    
    with open(machineEventsFile) as f:
        extractMachinesInfo(f, timeWindowSize)

if __name__ == "__main__":
    main(sys.argv[1:])

