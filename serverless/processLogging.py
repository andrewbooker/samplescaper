import datetime
import logging
import os
import sys

ts = datetime.datetime.now().strftime("%Y-%m-%d_%H%M%S")
log_fn = os.path.join(os.getenv("HOME"), "Documents", "logs", f"randomtone_serverless_{ts}.log")
logging.basicConfig(filename=log_fn, level=logging.INFO)

def createLog(module_name):
    log = logging.getLogger(module_name)
    sys.stdout.write = log.info
    sys.stderr.write = log.error
    return log
