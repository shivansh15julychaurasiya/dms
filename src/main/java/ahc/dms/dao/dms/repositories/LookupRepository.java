package ahc.dms.dao.dms.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import ahc.dms.dao.dms.entities.Lookup;

@Repository
public interface LookupRepository extends JpaRepository<Lookup, Long> {

    // Custom finder methods (optional and based on your use-case)

   
//    
//    Optional<Lookup> findBySetnameAndRecStatus(String setname, Integer recStatus);
//
//    
    @Query("SELECT l FROM Lookup l WHERE l.setname = :setname AND l.recStatus = :recStatus")
    Optional<Lookup> findBySetnameAndRecStatus(@Param("setname") String setname, @Param("recStatus") Integer recStatus);

    
    

    // Add more methods as needed based on your querying needs
}
