package ahc.dms.dao.respositories;

import ahc.dms.dao.entities.Category;
import ahc.dms.dao.entities.Post;
import ahc.dms.dao.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PostRepository extends JpaRepository<Post, Integer> {

    List<Post> findByUser(User user);
    List<Post> findByCategory(Category category);
    List<Post> findByTitleContaining(String title);

}
