package ahc.dms.dao.dms.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Product {
	
   @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
   @Column(name="pt_id")
   private Long id;
   
   @Column(name="pt_name")
   private String name;
  
   
   @Column(name="pt_description",length=2000)
   private String description;
   
   @Column(name="pt_price")
   private Double price;
   
   @Column(name="pt_stock")
   private Integer stock;
   
   @Column(name="pt_image_url")
   private String imageUrl; // path or URL


   @ManyToOne
   private Category category;


   @ManyToOne
   private SubCategory subCategory;

}
