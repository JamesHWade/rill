"""Model a pooler acknowledging cancellation before forwarding it upstream.

Delaying only CancelRequest packets makes an unread SELECT result cancel the
next query. Ordinary query traffic passes through unchanged on loopback.
"""

import argparse
import asyncio
import struct

parser = argparse.ArgumentParser()
parser.add_argument('--backend-port', type=int, required=True)
args = parser.parse_args()

async def forward_cancel(packet):
    await asyncio.sleep(0.003)
    reader, writer = await asyncio.open_connection('127.0.0.1', args.backend_port)
    writer.write(packet)
    await writer.drain()
    await reader.read()
    writer.close()
    await writer.wait_closed()

async def copy(reader, writer):
    try:
        while data := await reader.read(65536):
            writer.write(data)
            await writer.drain()
    except (ConnectionError, asyncio.CancelledError):
        pass
    finally:
        writer.close()

async def client(reader, writer):
    try:
        header = await reader.readexactly(8)
        length, code = struct.unpack('!II', header)
        if code == 80877102:
            packet = header + await reader.readexactly(length - 8)
            print('CANCEL_REQUEST', flush=True)
            writer.close()
            await writer.wait_closed()
            asyncio.create_task(forward_cancel(packet))
            return
        backend_reader, backend_writer = await asyncio.open_connection('127.0.0.1', args.backend_port)
        backend_writer.write(header)
        await backend_writer.drain()
        await asyncio.gather(copy(reader, backend_writer), copy(backend_reader, writer))
    except (asyncio.IncompleteReadError, ConnectionError):
        writer.close()

async def main():
    server = await asyncio.start_server(client, '127.0.0.1', 0)
    print(server.sockets[0].getsockname()[1], flush=True)
    async with server:
        await server.serve_forever()

asyncio.run(main())
