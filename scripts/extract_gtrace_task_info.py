#!/usr/bin/python

from __future__ import print_function
from collections import namedtuple
import sys

tasks = {}

TaskEvent = namedtuple('TaskEvent', ['time', 'jobId', 'taskId', 'type', 'user',
                                     'schedulingClass', 'priority', 'cpuReq',
                                     'memReq'])

class TaskInfo:
    def __init__(self, submitTime, user, schedulingClass, priority):
        self.submitTime = submitTime
        self.user = user
        self.schedulingClass = schedulingClass
        self.priority = priority
        self.cpuReq = 0
        self.memReq = 0
        self.runtime = 0 
        self.endTime = -1
        self.lastScheduled = -1

    def updateRequestedResources(self, cpuReq, memReq):
        try:
            self.cpuReq = max(self.cpuReq, float(cpuReq))
        except ValueError:
            pass    
        try:
            self.memReq = max(self.memReq, float(memReq))
        except ValueError:
            pass
    
    def updateRuntime(self, time):
        if self.isScheduled():
            self.runtime += time - self.lastScheduled
            self.lastScheduled = -1

    def isScheduled(self):
        return self.lastScheduled != -1

def extractTasksInfo(f, output):
    global tasks
    time = 0
    for line in f:
        token = line.split(',')
        event = TaskEvent(token[0], token[2], token[3], token[5], token[6],
                          token[7], token[8], token[9], token[10])  
        key = (event.jobId, event.taskId)

        if event.type == '0': # submitted
            time = int(event.time)
            if key not in tasks:
                tasks[key] = TaskInfo(event.time, event.user,
                                      event.schedulingClass, event.priority)
            else:
                tasks[key].endTime = -1
        elif event.type == '1': # scheduled
            time = int(event.time)
            task = tasks[key]
            task.lastScheduled = time 
            task.updateRequestedResources(event.cpuReq, event.memReq)
        elif event.type == '4': # finished
            time = int(event.time)
            task = tasks[key]
            task.runtime += time - task.lastScheduled
            task.endTime = time
            printTaskInfo(key, task, output)
            del tasks[key]
        elif event.type in ['2', '3', '5', '6']: # evicted, failed, killed, lost 
            time = int(event.time)
            task = tasks[key]
            task.updateRuntime(time)
            task.endTime = time
        else: # info updates
            task = tasks[key]
            task.updateRequestedResources(event.cpuReq, event.memReq)
    return time 

def printTaskInfo(taskKey, task, output):
    line = ' '.join(map(str, [task.submitTime, taskKey[0], taskKey[1],
                              task.user, task.schedulingClass, task.priority,
                              task.runtime, task.endTime, task.cpuReq,
                              task.memReq])) + '\n'
    output.write(line)

def printRemainingTasks(maxTime, output):
    for key, task in tasks.iteritems():
        task.updateRuntime(maxTime) 
        printTaskInfo(key, task, output)

def main(argv):
    maxTime = 0
    output = open("task-events.txt", "w")
    for inputFile in argv:
        with open(inputFile,"r") as f:
            maxTime = extractTasksInfo(f, output)

    printRemainingTasks(maxTime + 1, output)
    output.close()

if __name__ == "__main__":
    main(sys.argv[1:])

