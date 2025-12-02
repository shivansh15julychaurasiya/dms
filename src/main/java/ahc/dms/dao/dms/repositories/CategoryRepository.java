package ahc.dms.dao.dms.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import ahc.dms.dao.dms.entities.Category;

public interface CategoryRepository extends JpaRepository<Category, Long> {
	
	Optional<Category> findByNameIgnoreCase(String name);

}

