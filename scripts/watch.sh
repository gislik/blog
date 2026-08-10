#!/bin/bash

PORT=${PORT:-8000}

stack run blog watch -- --host 0.0.0.0 --port $PORT
