from flask import Flask, request, render_template
import sia
import json
import os
import argparse

app = Flask(__name__)

b = None
items = []
if os.path.exists("items.json"):
    with open("items.json", "r") as ifile:
        items = json.load(ifile)

@app.route('/')
def index():
    c = request.cookies.get('user')
    print("cookie-user:", c)
    ctx = json.loads(request.cookies.get('user'))
    item_idx = b.get_items(ctx)
    return render_template('page.html', items=[items[i] for _, i in enumerate(item_idx)])

@app.route('/click<int:item><int:position><float:reward>',methods=['GET', 'POST'])
def click(item, position, reward=1.0):
    ctx = json.loads(request.cookies.get('user'))
    print("handle_user_action called")
    b.handle_user_action(ctx, item, position, reward)
    return 'success'

@app.route('/handle_user_action<string:context><int:item><int:position><float:reward>',methods=['GET', 'POST'])
def handle_user_action(context, item, position, reward):
    print("handle_user_action called")
    ctx = json.loads(context)
    b.handle_user_action(ctx, item, position, reward)
    return 'success'

@app.route('/get_items<string:context><int:item><int:position>',methods=['GET', 'POST'])
def get_items(context, item, position):
    print("get items called")
    ctx = json.loads(context)
    # items = b.get_items(ctx, item, position)
    return json.dumps(items)



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run bandit')
    parser.add_argument('-items', type=int, default=3, help='Number of items')
    parser.add_argument('-positions', type=int, default=2, help='Number of positions')
    parser.add_argument('-contexts', type=int, default=3, help='Number of possible contexts')
    args = parser.parse_args()
    b = sia.Bandit(num_items=args.items, num_positions=args.positions, len_contexts=args.contexts)
    app.run(host='localhost', port=8080)