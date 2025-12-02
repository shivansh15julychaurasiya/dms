package ahc.dms.dao.dms.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


@Entity
@Getter 
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class SubCategory {
	
   @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
   @Column(name="scg_id")
   private Long id;
   @Column(name="scg_name")
   private String name;
   @ManyToOne
   private Category category;


}