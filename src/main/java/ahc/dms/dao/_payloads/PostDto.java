package ahc.dms.dao._payloads;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class PostDto {

    private int postId;
    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String title;
    @NotBlank
    @Size(min=10, message = "Must be greater than 10 characters.")
    private String content;
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String imageName;
    private Date addedDate;

    private CategoryDto categoryDto;
    private UserDto userDto;
    private Set<CommentDto> comments = new HashSet<>();

}
