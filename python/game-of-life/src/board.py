from itertools import product


class Board(object):
    def __init__(self, cell_array):
        self.living_cells = set()
        for p in self.__get_only_living_cells(cell_array):
            self.set_alive_at(p)

    def count_living_neighbours_of(self, position):
        (x, y) = position
        all_neighbours = [
            (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
            (x - 1, y), (x + 1, y),
            (x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
        ]
        return self.__length_of(filter(
            lambda p: self.is_alive_at(p),
            all_neighbours))

    def is_alive_at(self, position):
        return position in self.living_cells

    def set_alive_at(self, position):
        self.living_cells.add(position)

    def set_dead_at(self, position):
        if self.is_alive_at(position):
            self.living_cells.remove(position)

    def transform_cell_at(self, position, next_state):
        self.__get_transformation(next_state)(position)

    def dimensions(self):
        if len(self.living_cells) == 0:
            return 0, 0
        widest_living_cell = max(self.living_cells, key=lambda p: p[0])[0]
        highest_living_cell = max(self.living_cells, key=lambda p: p[1])[1]
        return widest_living_cell + 1, highest_living_cell + 1

    @staticmethod
    def __get_dimension(cell_array):
        height = len(cell_array)
        width = len(cell_array[0]) if height > 0 else 0
        return width, height

    def to_cell_array(self):
        width, height = self.dimensions()
        dump = [[self.__dump_value(x, y) for x in range(width)] for y in range(height)]
        return dump

    @staticmethod
    def __length_of(filtered):
        return len(list(filtered))

    def __get_transformation(self, next_state):
        return self.set_alive_at if next_state else self.set_dead_at

    def __get_only_living_cells(self, cell_array):
        width, height = self.__get_dimension(cell_array)
        matrix = product(range(0, width), range(0, height))
        alive_matrix = filter(lambda x_y: cell_array[x_y[1]][x_y[0]] == 'x', matrix)
        return alive_matrix

    def __dump_value(self, x, y):
        return 'x' if self.is_alive_at((x, y)) else '.'
