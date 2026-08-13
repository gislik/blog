#!/bin/bash

PORT=${PORT:-8000}
export DRAFTS="true"

stack run blog watch -- --host 0.0.0.0 --port $PORT
