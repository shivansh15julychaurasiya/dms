package ahc.dms.payload;

import jakarta.validation.constraints.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

@NoArgsConstructor
@Getter
@Setter
public class UserDto {

    private int userId;

    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String name;

    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String loginId;

    @Email(message = "Email address not valid")
    private String email;

    //@Pattern(regexp = "^(?=.*[A-Za-z])(?=.*\\d)(?=.*[@$!%*#?&])[A-Za-z\\d@$!%*#?&]{8,}$")
    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String password;

    @NotBlank
    private String about;

    private Set<RoleDto> roles = new HashSet<>();
}
