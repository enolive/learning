from itertools import product

from src.board import Board


class NextGeneration(object):
    @staticmethod
    def calculate(board):
        width, height = board.dimensions()
        new_board = Board([])
        for position in product(range(width + 1), range(height + 1)):
            NextGeneration.__set_next_state(board, new_board, position)
        return new_board

    @staticmethod
    def __set_next_state(board, new_board, position):
        living_neighbours = board.count_living_neighbours_of(position)
        current_state = board.is_alive_at(position)
        next_state = NextGeneration.__get_next_living_state(living_neighbours, current_state)
        new_board.transform_cell_at(position, next_state)

    @staticmethod
    def __get_next_living_state(living_neighbours, current_state):
        if living_neighbours == 2:
            return current_state
        elif living_neighbours == 3:
            return True
        else:
            return False
