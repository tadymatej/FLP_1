# Název výstupního souboru
OUTPUT_NAME = flp-fun

# Zdrojové soubory
SRCS = flp-fun.hs

# Cíle
all: $(OUTPUT_NAME)

# Generování objektových souborů
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.hs
	@mkdir -p $(OBJ_DIR)
	ghc -c $< -o $@

# Spojení objektových souborů do výstupního programu
$(OUTPUT_NAME): $(patsubst $(SRC_DIR)/%.hs,$(OBJ_DIR)/%.o,$(SRCS))
	ghc $^ -Wall -o $@

# Cíl pro čištění
clean:
	rm -rf $(OBJ_DIR) $(OUTPUT_NAME)

# Nastavení implicitních cílů
.PHONY: all clean