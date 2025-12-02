package ahc.dms.controller;

import java.util.List;
import java.util.Optional;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import ahc.dms.dao.dms.entities.Category;
import ahc.dms.dao.dms.repositories.CategoryRepository;
import ahc.dms.payload.dto.CategoryDTO;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/grocify/categories")
public class CategoryController {

    private final CategoryRepository repo;

    public CategoryController(CategoryRepository repo) {
        this.repo = repo;
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<?> create(@Valid @RequestBody CategoryDTO dto) {

        // unique name validation
        Optional<Category> existing = repo.findByNameIgnoreCase(dto.getName());
        if (existing.isPresent()) {
            return ResponseEntity.badRequest().body("Category already exists");
        }

        Category c = new Category();
        c.setName(dto.getName());
        return ResponseEntity.ok(repo.save(c));
    }

    @GetMapping
    public List<Category> list() {
        return repo.findAll();
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<?> delete(@PathVariable Long id) {

        try {
            if (!repo.existsById(id)) {
                return ResponseEntity.badRequest().body("Category not found");
            }
            repo.deleteById(id);
            return ResponseEntity.ok("Category deleted successfully");
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body("Unable to delete category");
        }
    }
}
