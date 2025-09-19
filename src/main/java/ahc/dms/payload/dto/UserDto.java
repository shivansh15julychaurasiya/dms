package ahc.dms.payload.dto;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@NoArgsConstructor
@Getter
@Setter
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDto {

    @JsonIgnore
    private Long version;

    @JsonProperty("user_id")
    private Long userId;

    @NotBlank
    @Size(min = 4, message = "Must be greater than 4 characters.")
    private String name;

    @NotBlank
    @Size(min = 4, message = "Must be greater than 4 characters.")
    private String username;
    
    @NotBlank
    @Size(min = 4, message = "Must be greater than 4 characters.")
    private String fullName;

    @Email(message = "Email address not valid")
    private String email;

    @NotBlank
    @Size(min = 4, message = "Must be greater than 4 characters.")
    private String password;

    @NotBlank
    private String phone;

    @NotBlank
    private String about;

    private Boolean status;

 

    // Convenience field for frontend: roles extracted from UserRole
    @JsonProperty("roles")
    private Set<LookupDto> roles = new HashSet<>();

    // Audit fields
    @JsonProperty("created_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime createdAt;

    @JsonProperty("updated_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime updatedAt;

    // --- Custom getters/setters ---
    @JsonIgnore
    public String getPassword() {
        return this.password;
    }

    @JsonProperty
    public void setPassword(String password) {
        this.password = password;
    }
}
