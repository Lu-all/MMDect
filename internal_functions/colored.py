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
        print(f"{Fore.LIGHTBLACK_EX}", text, f"{Style.RESET_ALL}")
