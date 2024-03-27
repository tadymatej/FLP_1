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

run_tests: 
	cp ./flp-fun ./public
	python3 public/test_flp.py --test_type inference
	python3 public/test_flp.py --test_type training

# Cíl pro čištění
clean:
	rm -rf $(OBJ_DIR) $(OUTPUT_NAME)

zip: 
	zip xzalma00.zip Makefile flp-fun.hs

# Nastavení implicitních cílů
.PHONY: all clean