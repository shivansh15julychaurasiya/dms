package ahc.dms.dao.dms.repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import ahc.dms.dao.dms.entities.SubCategory;

public interface SubCategoryRepository extends JpaRepository<SubCategory, Long> {
	
    List<SubCategory> findByCategoryId(Long categoryId);
}
