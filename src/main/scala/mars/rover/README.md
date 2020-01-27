Solution:

- Uses a grid, which is a form of graph, to represent the places where the Rover can go
- Grid is unweighted
- Nodes neighbours are determined by their position in the grid
- Grid connects 'left', 'up', 'down, and 'right'
- Search the shortest path using Breadth-first Search

To be improved:

- Rover doesn't move on the diagonals. We could change that using A* search algorithm instead of BDF, for example
- Appending nodes to a list (when queueing nodes) can hurt performance when dealing with a large grid
- Logic to verify if node has been visited or added to the queue can also hurt performance
- Grid in the documentation could be better formatted and look more like a network (with its vertices/nodes) and less like a matrix.
