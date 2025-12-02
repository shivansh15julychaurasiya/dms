package ahc.dms.dao.dms.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import ahc.dms.dao.dms.entities.SubCategory;
import ahc.dms.dao.dms.repositories.SubCategoryRepository;

@Service
public class SubCategoryService {

    @Autowired
    private SubCategoryRepository subCategoryRepository;

    // Create / Add
    public SubCategory addSubCategory(SubCategory subCategory) {
        return subCategoryRepository.save(subCategory);
    }

    // Get all
    public List<SubCategory> getAllSubCategories() {
        return subCategoryRepository.findAll();
    }

    // Get by Category ID
    public List<SubCategory> getSubCategoriesByCategory(Long categoryId) {
        return subCategoryRepository.findByCategoryId(categoryId);
    }

    // Update
    public SubCategory updateSubCategory(Long id, SubCategory updated) {
        SubCategory sc = subCategoryRepository.findById(id).orElseThrow();
        sc.setName(updated.getName());
      
        sc.setCategory(updated.getCategory());
        return subCategoryRepository.save(sc);
    }

    // Delete
    public void deleteSubCategory(Long id) {
        subCategoryRepository.deleteById(id);
    }
}
