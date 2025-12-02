package ahc.dms.payload.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CategoryDTO {
    
    @NotBlank(message = "Category name cannot be empty")
    private String name;
}
