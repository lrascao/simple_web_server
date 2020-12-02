#!/usr/bin/env python3

import websocket
import argparse
import random
import json
import logging

try:
    import thread
except ImportError:
    import _thread as thread
import time

disconnect = False

logging.basicConfig(level=logging.DEBUG,
                    format='(%(threadName)-0s) %(message)s',)

def on_message(ws, message):
    global disconnect
    decoded = json.loads(message)
    if 'reply' in decoded:
        reply = decoded['reply']
        reply_type = reply['type']
        if (reply_type == 'version'):
            version = reply['version']
            print('[{}] version: {}'.format(thread.get_ident(), version), end='\r')
    if 'disconnect' in decoded:
        reason = decoded['disconnect']
        logging.debug('disconnecting due to: {}'.format(reason))
        disconnect = True

def on_error(ws, error):
    logging.debug("error: ", error)

def on_close(ws):
    logging.debug("### closed ###")

def on_open(ws):
    print("### opened ###")
    thread.start_new_thread(main_loop, (ws, None))

def main_loop(ws, *args):
    logging.debug("### main loop")
    while True:
        time.sleep(1)
        if (disconnect == True):
            logging.debug('disconnecting');
            ws.close()
            return;
        ws.send(json.dumps({'req': {'type': 'version'}}))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--endpoint', action='store',
                        help='endpoint')
    parser.add_argument('--account_id', action='store',
                        help='account_id')
    parser.add_argument('--token', action='store',
                        help='token')
    args = parser.parse_args()

    # websocket.enableTrace(True)
    uri = "ws://{}/v1/connection/{}".format(args.endpoint.replace('http://', ''), args.account_id)
    ws = websocket.WebSocketApp(uri,
                                header = {'x-app-token': args.token},
                                on_message = on_message,
                                on_error = on_error,
                                on_close = on_close)
    ws.on_open = on_open
    # keep a ping keep alive every 2 seconds
    ws.run_forever(ping_interval = 2)

