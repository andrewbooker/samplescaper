import datetime
from dateutil import tz
import logging
import os

ts = datetime.datetime.now(tz=tz.UTC).strftime("%Y-%m-%d_%H%M%S")
log_fn = os.path.join(os.getenv("HOME"), "Documents", "logs", f"randomtone_{ts}.log")
logging.basicConfig(filename=log_fn, level=logging.INFO)
