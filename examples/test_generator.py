import random

benign_blocks = [
    "# Benign block 1\n"
    "mov r13, [0x123]\n"
    "push r12\n"
    "mov r13, r12\n"
    "#Comment3\n"
    "shl r12, 8\n",

    "# Benign block 2\n"
    "mov [0x123], 7\n"
    "mov r13, [0x123]\n"
    "je start_code\n"
    "jne start_code\n"
    "jump3:\n"
    "jmp init\n"
    "mov r12, 10\n"
    "push 12\n"
    "pop r12\n",

    "# Benign block 3\n"
    "mov [0x123], 7\n"
    "mov rdx, rax\n"
    "mov r14, 0x1\n"
    "mov r13, r12\n"
    "shl r12, 80\n"
    "sub r12, r13\n"
    "mov r13, r12\n"
    "mov rsi, rsp\n"
    "mov rax, 0x1\n"
    "syscall\n",
]

signatures = [
    "# Additional jumps\n"
    "jump3:\n"
    "jmp init\n"
    "mov r12, 10\n"
    "jump2:\n"
    "mov [0x123], 7\n"
    "mov r13, [0x123]\n"
    "jmp jump3\n"
    "jump1:\n"
    "init:\n"
    "sub r12, r13\n"
    "mov r13, r12\n"
    "shl r12, 8\n"
    "jmp jump2\n",

    "# Basic rule appliance\n"
    "mov [123], 0x6477737361702FFF\n"
    "push [123]\n"
    "push 12\n"
    "pop r12\n"
    "push r12\n"
    "mov r13, 13\n",

    "# Comments and fake jumps\n"
    "sub r12, r13\n"
    "mov r13, r12\n"
    "#Comment3\n"
    "shl r12, 8 #abc\n"
    "mov [0x123], 7\n"
    "start_code:\n"
    "mov r13, [0x123]\n"
    "#Morecomments\n"
    "cmp r13, 10\n"
    "je start_code\n"
    "jne start_code\n"
    "mov r12, 10\n",

    "# Immediates\n"
    "sub r12, r13\n"
    "mov r13, r12\n"
    "shl r12, 80\n"
    "mov [0x123], 80\n"
    "init:\n"
    "mov r13, [291]\n"
    "cmp r13, 0x64\n"
    "jmp init\n"
    "mov r12, 0x64\n",

    "# Print to screen\n"
    "mov rdx, rax\n"
    "mov r14, 0x1\n"
    "mov rdi, 0x1\n"
    "mov rsi, rsp\n"
    "mov rax, 0x1\n"
    "syscall\n",

    "# Etc/passwd\n"
    "mov r12, 0x6477737361702FFF\n"
    "shr r12, 8\n"
    "mov [r13], r12\n"
    "push [r13] # Rule 3\n"
    "mov r12, 0xFFFFFFFF6374652F\n"
    "shl r12, 32\n"
    "push r12\n"
]


def insert_benign_block(set):
    i = random.randint(0, len(benign_blocks) - 1)
    while set.__contains__(i):
        i = random.randint(0, len(benign_blocks) - 1)
    set.add(i)
    return benign_blocks[i]


def insert_malicious_block(set):
    i = random.randint(0, len(signatures) - 1)
    while set.__contains__(i):
        i = random.randint(0, len(signatures) - 1)
    set.add(i)
    return signatures[i]


def create_program(number_benign, number_malicious):
    already_seen_benign = set()
    already_seen_signatures = set()
    crafted_blocks = []
    for i in range(number_benign):
        crafted_blocks.append(insert_benign_block(already_seen_benign))
    for i in range(number_malicious):
        crafted_blocks.append(insert_malicious_block(already_seen_signatures))
    random.shuffle(crafted_blocks)
    crafted_blocks.insert(0, "_start:\n")
    crafted_blocks = ''.join((str(x) for x in crafted_blocks))
    return crafted_blocks.replace("['", "").replace("', '", "").replace("']", "").replace("'\\n'", '\n')


print(create_program(2, 1), file=open("example_2-1.txt", "w"))
print(create_program(3, 0), file=open("example_3-0.txt", "w"))
print(create_program(1, 2), file=open("example_1-2.txt", "w"))
print(create_program(0, 3), file=open("example_0-3.txt", "w"))
