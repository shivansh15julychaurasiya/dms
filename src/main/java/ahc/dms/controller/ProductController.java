package ahc.dms.controller;

import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.http.MediaType;

import ahc.dms.dao.dms.entities.Product;
import ahc.dms.dao.dms.repositories.ProductRepository;
import ahc.dms.dao.dms.services.ProductService;

@RestController
@RequestMapping("/grocify/products")
public class ProductController {

    private final ProductService service;
    private final ProductRepository productRepository;

    @Value("${grocify.upload.dir}")
    private String uploadDir;

    public ProductController(ProductService service, ProductRepository pr) {
        this.service = service;
        this.productRepository = pr;
    }

    /** ---------------- CREATE PRODUCT ---------------- */
    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<?> create(
            @RequestParam(required = false) MultipartFile image,
            @RequestParam String name,
            @RequestParam String description,
            @RequestParam Double price,
            @RequestParam Integer stock,
            @RequestParam(required = false) Long categoryId,
            @RequestParam(required = false) Long subCategoryId
    ) throws IOException {


        // Validate image
        if (image != null && !image.isEmpty()) {
            if (!image.getContentType().startsWith("image/")) {
                return ResponseEntity.badRequest().body("Only image files allowed");
            }
            if (image.getSize() > (8 * 1024 * 1024)) { // 5MB max
                return ResponseEntity.badRequest().body("Image size too large");
            }
        }

        Product saved = service.create(image, name, description, price, stock, categoryId, subCategoryId);
        return ResponseEntity.ok(saved);
    }

    /** ---------------- LIST WITH PAGINATION & SEARCH ---------------- */
    @GetMapping
    public Page<Product> list(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sort,
            @RequestParam(defaultValue = "asc") String order,
            @RequestParam(required = false) String search
    ) {
        PageRequest pageable = PageRequest.of(page, size,
                order.equals("asc") ? Sort.by(sort).ascending() : Sort.by(sort).descending());

        if (search != null && !search.isEmpty()) {
            return productRepository.findByNameContainingIgnoreCase(search, pageable);
        }
        return productRepository.findAll(pageable);
    }

    /** ---------------- UPDATE PRODUCT ---------------- */
    @PutMapping(value = "/{id}", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<?> update(
            @PathVariable Long id,
            @RequestParam(required = false) MultipartFile image,
            @RequestParam String name,
            @RequestParam String description,
            @RequestParam Double price,
            @RequestParam Integer stock,
            @RequestParam(required = false) Long categoryId,
            @RequestParam(required = false) Long subCategoryId
    ) throws IOException {

        return productRepository.findById(id).map(existing -> {

            if (image != null && !image.isEmpty()) {
                try {
                    String filename = service.getFileStorageService().store(image);
                    existing.setImageUrl(filename);
                } catch (IOException e) {
                    return ResponseEntity.internalServerError().body("Error uploading file");
                }
            }

            existing.setName(name);
            existing.setDescription(description);
            existing.setPrice(price);
            existing.setStock(stock);

            if (categoryId != null)
                service.getCategoryRepository().findById(categoryId).ifPresent(existing::setCategory);
            if (subCategoryId != null)
                service.getSubCategoryRepository().findById(subCategoryId).ifPresent(existing::setSubCategory);

            return ResponseEntity.ok(productRepository.save(existing));
        }).orElse(ResponseEntity.badRequest().body("Product not found"));
    }

    /** ---------------- DELETE PRODUCT ---------------- */
    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<?> delete(@PathVariable Long id) {
        if (!productRepository.existsById(id)) {
            return ResponseEntity.badRequest().body("Product not found");
        }
        productRepository.deleteById(id);
        return ResponseEntity.ok("Product deleted");
    }

    /** ---------------- SERVE PRODUCT IMAGE ---------------- */
    @GetMapping("/image/{filename}")
    public ResponseEntity<Resource> getImage(@PathVariable String filename) throws MalformedURLException {
        Path file = Paths.get(uploadDir).resolve(filename).normalize();
        UrlResource resource = new UrlResource(file.toUri());

        if (!resource.exists()) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"" + filename + "\"")
                .body(resource);
    }
}
