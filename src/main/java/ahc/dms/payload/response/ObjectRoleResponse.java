package ahc.dms.payload.response;


import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import ahc.dms.payload.dto.LookupDto;
import ahc.dms.payload.dto.ObjectMasterDto;
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
public class ObjectRoleResponse {

    @JsonProperty("object")
    private ObjectMasterDto objectMasterDto;
    @JsonProperty("roles")
    private Set<LookupDto> roleDto;

}
