package ahc.dms.dao.dms.repositories;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.jpa.repository.JpaRepository;

import ahc.dms.dao.dms.entities.Product;

public interface ProductRepository extends JpaRepository<Product, Long> {

     List<Product> findBySubCategoryId(Long subCategoryId);
     
     Page<Product> findByNameContainingIgnoreCase(String name, org.springframework.data.domain.Pageable pageable);
     List<Product> findByCategoryId(Long id);

}
