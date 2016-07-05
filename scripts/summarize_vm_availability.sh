#!/bin/bash

if [ "$#" -le 0 ]; then
    echo "Error: Illegal number of parameters."
    echo "Usage: ./summarize_vm_availability.sh <availability files>" 
    exit
fi

Rscript src/summarize_vm_availability.R $* 
