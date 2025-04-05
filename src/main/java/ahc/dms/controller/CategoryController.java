package ahc.dms.controller;

import ahc.dms.dao._payloads.ApiResponse;
import ahc.dms.dao._payloads.CategoryDto;
import ahc.dms.dao.services.CategoryService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/blog/category")
public class CategoryController {

    @Autowired
    private CategoryService categoryService;

    @PostMapping("/")
    public ResponseEntity<CategoryDto> createCategory(@Valid @RequestBody CategoryDto categoryDto){

        CategoryDto createdCatDto = categoryService.createCategory(categoryDto);
        return new ResponseEntity<>(createdCatDto, HttpStatus.CREATED);

    }

    @PutMapping("/{catId}")
    public ResponseEntity<CategoryDto> updateCategory(@RequestBody CategoryDto catDto, @PathVariable("catId") Integer catId) {
        CategoryDto updatedCat = categoryService.updateCategory(catDto, catId);
        return ResponseEntity.ok(updatedCat);
    }

    @DeleteMapping("/{catId}")
    public ResponseEntity<ApiResponse> deleteCategory(@PathVariable("catId") Integer catId){
        categoryService.deleteCategory(catId);
        return new ResponseEntity<>(new ApiResponse("category deleted", true), HttpStatus.OK);
    }

    @GetMapping("/")
    public ResponseEntity<List<CategoryDto>> getAllCategories(){
        return ResponseEntity.of(Optional.ofNullable(categoryService.getAllCategories()));
    }

    @GetMapping("/{catId}")
    public ResponseEntity<CategoryDto> getSingleUser(@PathVariable("catId") Integer catId){
        return ResponseEntity.ok(categoryService.getCategory(catId));
    }

}
