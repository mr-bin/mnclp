import socket
import argparse
import kv_pb2


def connect(host, port):
    remote_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    remote_socket.connect((host, port))
    data = remote_socket.recv(1024)
    print('Received', repr(data))
    return remote_socket


def put(key, value):
    req_envelope = kv_pb2.req_envelope()
    req_envelope.type = 1
    data = req_envelope.set_req.req
    data.key = key
    data.value = value
    return req_envelope


def get(key):
    req_envelope = kv_pb2.req_envelope()
    req_envelope.type = 3
    req_envelope.get_req.key = key
    return req_envelope


def send(remote_socket, req_envelope):
    print(req_envelope)
    serialized_data_send = req_envelope.SerializeToString()
    remote_socket.sendall(serialized_data_send)


def receive(remote_socket):
    data = remote_socket.recv(1024)
    req_envelope = kv_pb2.req_envelope()
    req_envelope.ParseFromString(data)
    print(req_envelope)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='mnclp client')
    parser.add_argument('-a', '--host', dest='host', type=str, default=socket.gethostname())
    parser.add_argument('-p', '--port', dest='port', type=int, default=8787)
    parser.add_argument('-k', '--key', dest='key', type=str)
    parser.add_argument('-v', '--value', dest='value', type=str, default='')
    args = parser.parse_args()

    client_socket = connect(args.host, args.port)
    if args.key and args.value:
        request = put(args.key, args.value)
    else:
        request = get(args.key)
    send(client_socket, request)
    receive(client_socket)
    client_socket.close()
