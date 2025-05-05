package ahc.dms.payload;

import com.fasterxml.jackson.annotation.*;
import jakarta.persistence.Column;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

@NoArgsConstructor
@Getter
@Setter
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDto {

    @JsonIgnore
    private Long version;
    private Long userId;

    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String name;

    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    @JsonProperty("login_id")
    private String loginId;
    @Email(message = "Email address not valid")
    private String email;
    //@Pattern(regexp = "^(?=.*[A-Za-z])(?=.*\\d)(?=.*[@$!%*#?&])[A-Za-z\\d@$!%*#?&]{8,}$")
    @NotBlank
    @Size(min=4, message = "Must be greater than 4 characters.")
    private String password;
    @NotBlank
    private String phone;
    @NotBlank
    private String about;
    private Boolean status;

    @JsonProperty("user_roles")
    @NotEmpty(message = "At least one role must be specified")
    private Set<@Valid UserRoleDto> userRoles = new HashSet<>();

    @JsonProperty("roles")
    private Set<RoleDto> roles = new HashSet<>();

    // audit fields
    @JsonProperty("created_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime createdAt;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;

    // CUSTOM GETTERS AND SETTERS

    // HIDING PASSWORD
    @JsonIgnore
    public String getPassword(){
        return this.password;
    }

    @JsonProperty
    public void setPassword(String password){
        this.password = password;
    }
    
}
