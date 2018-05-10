from flask import Flask
import sia
import json
import argparse

app = Flask(__name__)

b = None

@app.route('/get_items/<string:context><int:item><int:position>')
def get_items(context, item, position):
    ctx = json.loads(context)
    items = b.get_items(ctx, item, position)
    return json.dumps(items)

@app.route('/handle_user_action/<string:context><int:item><int:position><float:reward>')
def handle_user_action(context, item, position, reward):
    ctx = json.loads(context)
    b.handle_user_action(ctx, item, position, reward)
    return 'success'

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run bandit')
    parser.add_argument('-items', type=int, default=3, help='Number of items')
    parser.add_argument('-positions', type=int, default=2, help='Number of positions')
    parser.add_argument('-contexts', type=int, default=3, help='Number of possible contexts')
    args = parser.parse_args()
    b = sia.Bandit(num_items=args.items, num_positions=args.positions, len_contexts=args.contexts)
    app.run(host='localhost', port=5000)