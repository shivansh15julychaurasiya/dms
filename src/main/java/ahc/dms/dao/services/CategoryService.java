package ahc.dms.dao.services;

import ahc.dms.dao.entities.Category;
import ahc.dms.dao._payloads.CategoryDto;
import ahc.dms.dao.respositories.CategoryRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import jakarta.transaction.Transactional;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class CategoryService {

    @Autowired
    private CategoryRepository categoryRepository;
    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public CategoryDto createCategory(CategoryDto categoryDto) {

        Category cat = modelMapper.map(categoryDto, Category.class);
        Category addedCat = categoryRepository.save(cat);
        return modelMapper.map(addedCat, CategoryDto.class);
    }

    @Transactional
    public CategoryDto updateCategory(CategoryDto categoryDto, Integer categoryId){
        Category cat = categoryRepository.findById(categoryId)
                .orElseThrow(() -> new ResourceNotFoundException("Category", "Id", categoryId));
        cat.setCategoryTitle(categoryDto.getCategoryTitle());
        cat.setCategoryDescription(categoryDto.getCategoryDescription());
        Category updatedCat = categoryRepository.save(cat);
        return modelMapper.map(updatedCat, CategoryDto.class);
    }

    @Transactional
    public void deleteCategory(Integer categoryId){
        Category cat = categoryRepository.findById(categoryId)
                .orElseThrow(() -> new ResourceNotFoundException("Category", "Id", categoryId));
        categoryRepository.delete(cat);
    }

    public CategoryDto getCategory(Integer categoryId) {
        Category cat = categoryRepository.findById(categoryId)
                .orElseThrow(() -> new ResourceNotFoundException("Category", "Id", categoryId));
        return modelMapper.map(cat, CategoryDto.class);

    }

    public List<CategoryDto> getAllCategories(){
        List<Category> categories = categoryRepository.findAll();
        return categories.stream()
                .map(category -> modelMapper.map(category, CategoryDto.class)).collect(Collectors.toList());
    }

}
