package ahc.dms.dao.dms.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter 
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Category {
	
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name="cg_id")
    private Long id;
    @Column(name="cg_name")
    private String name;
    @Column(name="cg_description")
    private String description;
    
    

}