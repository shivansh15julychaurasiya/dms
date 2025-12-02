package ahc.dms.controller;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import ahc.dms.dao.dms.entities.SubCategory;
import ahc.dms.dao.dms.services.SubCategoryService;

@RestController
@RequestMapping("/grocify/subcategories")
public class SubCategoryController {

    @Autowired
    private SubCategoryService subCategoryService;

    // CREATE
    @PostMapping
    public SubCategory add(@RequestBody SubCategory subCategory) {
        return subCategoryService.addSubCategory(subCategory);
    }

    // GET ALL
    @GetMapping
    public List<SubCategory> getAll() {
        return subCategoryService.getAllSubCategories();
    }

    // GET BY CATEGORY ID
    @GetMapping("/category/{categoryId}")
    public List<SubCategory> getByCategory(@PathVariable Long categoryId) {
        return subCategoryService.getSubCategoriesByCategory(categoryId);
    }

    // UPDATE
    @PutMapping("/{id}")
    public SubCategory update(@PathVariable Long id, @RequestBody SubCategory subCategory) {
        return subCategoryService.updateSubCategory(id, subCategory);
    }

    // DELETE
    @DeleteMapping("/{id}")
    public String delete(@PathVariable Long id) {
        subCategoryService.deleteSubCategory(id);
        return "SubCategory deleted successfully";
    }
}
