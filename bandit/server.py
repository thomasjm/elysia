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
    if len(item_idx) == 0:
        return render_template('page.html', items=items[:b.M])
    return render_template('page.html', items=[items[i] for _, i in enumerate(item_idx)])

@app.route('/click',methods=['GET', 'POST'])
def click():
    item = int(request.form['item'])
    position = int(request.form['position'])
    print("item:", item, "position:", position)
    ctx = json.loads(request.cookies.get('user'))
    b.handle_user_action(ctx, item, position)
    return 'success'

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run bandit')
    parser.add_argument('-items', type=int, default=3, help='Number of items')
    parser.add_argument('-positions', type=int, default=2, help='Number of positions')
    parser.add_argument('-contexts', type=int, default=3, help='Number of possible contexts')
    args = parser.parse_args()
    b = sia.Bandit(num_items=args.items, num_positions=args.positions, len_contexts=args.contexts)
    app.run(host='localhost', port=8080)