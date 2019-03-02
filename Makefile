
ROOT	=	.

NAME	=	deBruijn

CC		=	stack

V		=	@

all: $(NAME)

$(NAME):
	$(V)$(CC) build
	$(V)cp `stack path --local-install-root`/bin/$(NAME)-exe $(NAME)

clean:
	$(V)$(CC) clean

fclean: clean
	$(V)rm -rf $(NAME)

re: fclean all
