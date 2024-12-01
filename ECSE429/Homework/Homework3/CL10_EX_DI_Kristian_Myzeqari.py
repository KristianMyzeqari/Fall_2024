import sys
import random
from bitarray import bitarray

def read_binary_file(file_path):
    try:
        with open(file_path, "rb") as file:
            data = bytearray(file.read())
        return data
    except MemoryError:
        print("Error: The file is too large to be read into memory.")
        return None
    except FileNotFoundError:
        print("Error: File not found.")
        return None

def write_binary_file(file_path, data):
    try:
        with open(file_path, "wb") as file:
            file.write(data)
    except Exception as e:
        print(f"Error writing to file: {e}")

def fuzz_bytes(data, randomization_percentage):
    total_size = len(data)
    if total_size == 0:
        print("Error: Input file is empty.")
        return data

    # Convert byte array to a bitarray for easier manipulation
    bits = bitarray()
    bits.frombytes(data)

    # Calculate total bits and the number of bits to fuzz
    total_bits = len(bits)
    fuzz_target_bit_count = int((randomization_percentage / 100.0) * total_bits)

    # Randomly toggle bits
    for _ in range(fuzz_target_bit_count):
        bit_position = random.randint(0, total_bits - 1)
        bits[bit_position] = not bits[bit_position]  # Flip the bit

    # Convert back to bytearray
    fuzzed_data = bytearray(bits.tobytes())
    return fuzzed_data

def main():
    if len(sys.argv) != 4:
        print("Usage: python Fuzzer_Alternative.py <input_file> <output_file> <randomization_percentage>")
        return

    input_file_name = sys.argv[1]
    output_file_name = sys.argv[2]
    randomization_percentage = float(sys.argv[3])

    try:
        # Read the binary file into a byte array
        original_data = read_binary_file(input_file_name)

        if original_data is not None:
            # Fuzz the byte array
            fuzzed_data = fuzz_bytes(original_data, randomization_percentage)

            # Write the fuzzed data to the output file
            write_binary_file(output_file_name, fuzzed_data)
            print(f"Fuzzing completed. Output written to {output_file_name}")
    except ValueError:
        print("Error: Randomization percentage must be a valid number.")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

if __name__ == "__main__":
    main()

