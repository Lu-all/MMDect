import random

from colorama import Fore, Style


def prints(text: str, silent: bool) -> None:
    if not silent:
        print(text)


def print_error(text: str, silent: bool) -> None:
    if not silent:
        print(f"{Fore.RED}", text, f"{Style.RESET_ALL}")


def print_pass(text: str, silent: bool) -> None:
    if not silent:
        print(f"{Fore.GREEN}", text, f"{Style.RESET_ALL}")


def print_info(text: str, silent: bool) -> None:
    if not silent:
        print(f"{Fore.BLUE}", text, f"{Style.RESET_ALL}")


def print_warn(text: str, silent: bool) -> None:
    if not silent:
        print(f"{Fore.YELLOW}", text, f"{Style.RESET_ALL}")


def print_banner(text: str, silent: bool) -> None:
    if not silent:
        colours = {
            1: Fore.RED,
            2: Fore.GREEN,
            3: Fore.BLUE,
            4: Fore.YELLOW,
            5: Fore.LIGHTBLACK_EX,
            6: Fore.CYAN,
            7: Fore.MAGENTA,
            8: Fore.RED
        }
        r = random.randint(1, len(colours))
        print(f"{colours[r]}", text, f"{Style.RESET_ALL}")
