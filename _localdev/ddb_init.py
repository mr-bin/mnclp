
from boto.dynamodb2.fields import HashKey, RangeKey
from boto.dynamodb2.layer1 import DynamoDBConnection
from boto.dynamodb2.table import Table
from boto.dynamodb2.types import STRING, BINARY


def boto_ddb_connection():
    conn = DynamoDBConnection(
        region='us-east-1',
        aws_access_key_id='_not_needed_locally_',
        aws_secret_access_key='_not_needed_locally_',
        host="127.0.0.1",
        port=4566,
        is_secure=False)
    return conn


def create_ddb_table(table_name, schema):
    conn = boto_ddb_connection()

    table_schema = []
    for one in schema:
        if one['AttributeType'] == "B":
            table_key_type = BINARY
        if one['AttributeType'] == "S":
            table_key_type = STRING

        if one['KeyType'] == 'HASH':
            table_key = HashKey(one['AttributeName'], data_type=table_key_type)
        if one['KeyType'] == 'RANGE':
            table_key = RangeKey(one['AttributeName'], data_type=table_key_type)
        table_schema.append(table_key)

    table = Table.create(table_name,
                         schema=table_schema,
                         connection=conn)

    return table


if __name__ == '__main__':
    tables = [
        {"table_name": "test_table",
         "schema": [{"AttributeName": "key",
                     "KeyType": "HASH",
                     "AttributeType": "B"}]
         }
    ]

    for table in tables:
        create_ddb_table(table['table_name'], table['schema'])
