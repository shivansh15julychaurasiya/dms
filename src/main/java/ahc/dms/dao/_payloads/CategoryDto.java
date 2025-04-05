package ahc.dms.dao._payloads;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
@Setter
public class CategoryDto {

    private Integer categoryId;
    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String categoryTitle;
    @NotBlank
    @Size(min=4, message = "Must be greater than 10 characters.")
    private String categoryDescription;

}
