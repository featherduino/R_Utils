library(stringr)

clean_urls <- c("https://www.vianaar.com/la-zorita-3bhk-villas-goa-sale.php",
                "https://www.vianaar.com/el-rocio-2bhk-apartments-goa.php",
                "https://www.vianaar.com/la-morena-4bhk-villas-goa-sale.php",
                "https://www.vianaar.com/la-zorita-3bhk-villas-goa-sale.php",
                "http://www.vianaar.com/la-olalian-estate-2bhk-villas-goa",
                "https://www.vianaar.com/la-donaira-2-3bhk-villas-goa-sale.php",
                "https://www.vianaar.com/la-meira-3bhk-villas-goa.php",
                "https://www.vianaar.com/la-meira-3bhk-villas-goa.php",
                "https://www.vianaar.com/la-pasado-estate-4bhk-villas-goa.php",
                "https://www.vianaar.com/la-avila-2bhk-villas-goa.php",
                "https://www.vianaar.com/el-rocio-2bhk-apartments-goa.php",
                "https://www.vianaar.com/la-macedo-3bhk-villas-goa.php",
                "https://www.vianaar.com/el-arbol-2bhk-apartments-goa.php",
                "https://www.vianaar.com/manera-4bhk-villa-goa.php",
                "https://www.vianaar.com/la-branca-4bhk-villas-goa-sale.php",
                "https://www.vianaar.com/homes.php",
                "https://www.vianaar.com/el-volar-2bhk-apartments-goa.php",
                "https://www.vianaar.com/la-estada-estate-4bhk-villas-goa.php",
                "https://www.vianaar.com/la-cassinella-4bhk-villas-goa-sale.php",
                "https://www.vianaar.com/rentals-la-maroma-estate.php",
                "https://www.vianaar.com/la-merida-4bhk-villas-goa-sale.php",
                "https://www.vianaar.com/rentals.php",
                "http://www.vianaar.com/homes.php#villas",
                "http://www.vianaar.com/m/el-raso-1bhk-apartments-north-goa-for-sale",
                "http://www.vianaar.com/el-raso-1bhk-apartments-north-goa-for-sale",
                "http://www.vianaar.com/el-reino-2-apartments-for-sale-goa",
                "http://www.vianaar.com/el-reino-apartments-for-sale-goa",
                "http://www.vianaar.com/m/la-olalian-estate-2bhk-villas-goa",
                "http://www.vianaar.com/m/la-orilla-estate-3bhk-villas-goa",
                "https://www.vianaar.com/la-orilla-estate-3bhk-villas-goa.php",
                "http://www.vianaar.com/m/el-reino-2-apartments-for-sale-goa",
                "http://www.vianaar.com/la-seirra-3-bhk-villas-for-sale-in-north-goa",
                "http://www.vianaar.com/la-olalian-estate-2bhk-villas-goa",
                "http://www.vianaar.com/el-raso-1bhk-apartments-north-goa-for-sale",
                "http://www.vianaar.com/el-raso-1bhk-apartments-north-goa-for-sale",
                "https://www.vianaar.com/la-avila-2bhk-villas-goa.php",
                "http://www.vianaar.com/la-orilla-estate-3bhk-villas-goa",
                "https://www.vianaar.com/la-pasado-estate-4bhk-villas-goa.php",
                "http://www.vianaar.com/el-reino-2-apartments-for-sale-goa",
                "http://www.vianaar.com/el-reino-apartments-for-sale-goa",
                "https://www.vianaar.com/la-meira-3bhk-villas-goa.php",
                "https://www.vianaar.com/la-branca-4bhk-villas-goa-sale.php",
                "https://www.vianaar.com/homes.php",
                "https://www.vianaar.com/el-volar-2bhk-apartments-goa.php",
                "https://www.vianaar.com/la-orilla-estate-3bhk-villas-goa.php",
                "https://www.vianaar.com/la-macedo-3bhk-villas-goa.php",
                "http://www.vianaar.com/la-seirra-3-bhk-villas-for-sale-in-north-goa",
                "http://www.vianaar.com/homes.php",
                "http://www.vianaar.com/m/el-raso-1bhk-apartments-north-goa-for-sale",
                "https://www.vianaar.com/la-orilla-estate-3bhk-villas-goa",
                "https://www.vianaar.com/la-meira-3bhk-villas-goa.php",
                "https://www.vianaar.com/la-pasado-estate-4bhk-villas-goa.php",
                "http://www.vianaar.com/la-orilla-estate-3bhk-villas-goa",
                "http://www.vianaar.com/el-raso-1bhk-apartments-north-goa-for-sale",
                "http://www.vianaar.com/la-seirra-3-bhk-villas-for-sale-in-north-goa",
                "https://www.vianaar.com/la-pasado-estate-4bhk-villas-goa.php")

# Remove duplicate URLs from clean_urls
clean_urls <- unique(clean_urls)

# Function to extract subdomain from a URL
extract_subdomain <- function(url) {
  subdomain_match <- str_match(url, "https?://([[:alnum:].-]+)/")[, 3]
  ifelse(is.na(subdomain_match), "", subdomain_match)
}

# Extract subdomains from clean_urls
clean_subdomains <- sapply(clean_urls, extract_subdomain)

# Function to map clean URLs to messy URLs based on subdomains
map_urls_to_clean_urls_subdomains <- function(messy_urls, clean_subdomains) {
  subdomains <- sapply(messy_urls, extract_subdomain)
  matching_indices <- match(subdomains, clean_subdomains)
  return(clean_urls[matching_indices])
}

# Example usage:
messy_urls <- c("https://vianaar.comvianaar.com",
                "https://www.vianaar.com/la-dora-3bhk-villas-goa-sale.php?gclid=CjwKCAjw_o-HBhAsEiwANqYhp7LSFDPm4FlqP7M_Q6_7PC7EN6jW9PqjEIvTr1LaJPb-el7UtA0GfRoC3TcQAvD_BwE")

matched_clean_urls <- map_urls_to_clean_urls_subdomains(messy_urls, clean_subdomains)
print(matched_clean_urls)