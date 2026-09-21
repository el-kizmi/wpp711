import pdfplumber
import os
import re

folder = r"C:\temp\romie.jhonnerie\jurnal\amanisal"
output_file = os.path.join(folder, "all_abstracts.txt")

with open(output_file, "w", encoding="utf-8") as out:
    for i in range(2, 12):
        filename = f"document ({i}).pdf"
        filepath = os.path.join(folder, filename)
        if not os.path.exists(filepath):
            continue
        try:
            with pdfplumber.open(filepath) as pdf:
                full_text = ""
                for page in pdf.pages:
                    text = page.extract_text()
                    if text:
                        full_text += text + "\n"
                
                # Extract abstract
                abstract = ""
                # Try to find abstract section
                patterns = [
                    r'(?i)abstract[:\s]*\n(.*?)(?=\n\s*(?:introduction|keywords|introduksi|kata kunci|1\.|bab 1))',
                    r'(?i)abstrak[:\s]*\n(.*?)(?=\n\s*(?:introduction|keywords|introduksi|kata kunci|1\.|bab 1))',
                    r'(?i)ABSTRACT[:\s]*(.*?)(?=Keywords|INTRODUCTION|I\.\s|INTRODUCTION)',
                ]
                
                for pattern in patterns:
                    match = re.search(pattern, full_text, re.DOTALL | re.IGNORECASE)
                    if match:
                        abstract = match.group(1).strip()
                        break
                
                # If no abstract found, take first ~500 chars as approximation
                if not abstract:
                    # Look for common abstract markers
                    lines = full_text.split('\n')
                    start_idx = 0
                    end_idx = min(30, len(lines))
                    for j, line in enumerate(lines):
                        if re.search(r'(?i)abstract|abstrak', line):
                            start_idx = j + 1
                            for k in range(j+1, min(j+30, len(lines))):
                                if re.search(r'(?i)keyword|introduction|introduksi|1\.|bab 1', lines[k]):
                                    end_idx = k
                                    break
                            break
                    abstract = '\n'.join(lines[start_idx:end_idx])
                
                out.write(f"=== DOCUMENT {i} ===\n")
                out.write(f"Source: {filename}\n")
                out.write(f"Abstract:\n{abstract}\n\n")
                out.write("=" * 80 + "\n\n")
                
                # Also save full text for reference
                full_text_file = os.path.join(folder, f"full_text_{i}.txt")
                with open(full_text_file, "w", encoding="utf-8") as ft:
                    ft.write(full_text)
                    
                print(f"Processed {filename}: abstract length = {len(abstract)} chars")
        except Exception as e:
            print(f"Error processing {filename}: {e}")

print(f"\nDone! All abstracts saved to {output_file}")
