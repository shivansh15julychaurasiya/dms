package ahc.dms.dao.dms.repositories;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import ahc.dms.dao.dms.entities.Lookup;

@Repository
public interface LookupRepository extends JpaRepository<Lookup, Long> {

//     Find one by setname and recStatus
    @Query("SELECT l FROM Lookup l WHERE l.setname = :setname AND l.recStatus = :recStatus")
    List<Lookup> findListBySetnameAndRecStatus(@Param("setname") String setname,
            @Param("recStatus") Integer recStatus);
    
    

//     Full paginated + sorted version
    @Query(
        value = "SELECT l FROM Lookup l WHERE l.setname = 'DMS_ROLE' "
        
    )
    Page<Lookup> findAllRolesFromLookup(  Pageable pageable);

 

 // Non-paginated version
    @Query("SELECT l FROM Lookup l WHERE l.setname = 'DMS_ROLE'")
    List<Lookup> findAllRolesFromLookup();
    
    
    // Optional lookup by setname and longname
    List<Lookup> findBySetnameAndLongname(String setname, String longname);
    
    // check if role already exists (by longname & setname)
    boolean existsByLongnameAndSetname(String longname, String setname);
}
