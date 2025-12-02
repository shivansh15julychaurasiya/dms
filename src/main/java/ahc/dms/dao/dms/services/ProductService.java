package ahc.dms.dao.dms.services;

import java.io.IOException;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import ahc.dms.dao.dms.entities.Product;
import ahc.dms.dao.dms.entities.Category;
import ahc.dms.dao.dms.entities.SubCategory;
import ahc.dms.dao.dms.repositories.CategoryRepository;
import ahc.dms.dao.dms.repositories.ProductRepository;
import ahc.dms.dao.dms.repositories.SubCategoryRepository;
import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ProductService {

    private final ProductRepository productRepository;
    private final CategoryRepository categoryRepository;
    private final SubCategoryRepository subCategoryRepository;
    private final FileStorageService fileStorageService;

    /** ---------------- CREATE PRODUCT ---------------- */
    public Product create(
            MultipartFile image,
            String name,
            String desc,
            Double price,
            Integer stock,
            Long categoryId,
            Long subCategoryId
    ) throws IOException {

        Product p = new Product();
        mapProductFields(p, image, name, desc, price, stock, categoryId, subCategoryId);
        return productRepository.save(p);
    }

    /** ---------------- UPDATE PRODUCT ---------------- */
    public Product update(
            Long productId,
            MultipartFile image,
            String name,
            String desc,
            Double price,
            Integer stock,
            Long categoryId,
            Long subCategoryId
    ) throws IOException {

        Product existing = productRepository.findById(productId)
                .orElseThrow(() -> new RuntimeException("Product not found"));

        mapProductFields(existing, image, name, desc, price, stock, categoryId, subCategoryId);
        return productRepository.save(existing);
    }

    /** ---------------- INTERNAL PRODUCT MAPPING ---------------- */
    private void mapProductFields(
            Product p,
            MultipartFile image,
            String name,
            String desc,
            Double price,
            Integer stock,
            Long categoryId,
            Long subCategoryId
    ) throws IOException {

        p.setName(name);
        p.setDescription(desc);
        p.setPrice(price);
        p.setStock(stock);

        if (categoryId != null) {
            Category category = categoryRepository.findById(categoryId)
                    .orElseThrow(() -> new RuntimeException("Invalid category"));
            p.setCategory(category);
        }

        if (subCategoryId != null) {
            SubCategory subCategory = subCategoryRepository.findById(subCategoryId)
                    .orElseThrow(() -> new RuntimeException("Invalid subcategory"));
            p.setSubCategory(subCategory);
        }

        if (image != null && !image.isEmpty()) {
            String filename = fileStorageService.store(image);
            p.setImageUrl(filename);
        }
    }

    /** Optional getters if Controller needs them */
    public ProductRepository getProductRepository() { return productRepository; }
    public CategoryRepository getCategoryRepository() { return categoryRepository; }
    public SubCategoryRepository getSubCategoryRepository() { return subCategoryRepository; }
    public FileStorageService getFileStorageService() { return fileStorageService; }
}
