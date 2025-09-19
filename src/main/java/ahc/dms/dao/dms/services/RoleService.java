package ahc.dms.dao.dms.services;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ahc.dms.dao.dms.entities.Lookup;

import ahc.dms.dao.dms.entities.User;
import ahc.dms.dao.dms.repositories.LookupRepository;
import ahc.dms.exceptions.ApiException;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.dto.LookupDto;
import ahc.dms.payload.dto.RoleDto;
import ahc.dms.payload.response.PageResponse;

@Service
public class RoleService {

//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
    @Autowired
    private final LookupRepository lookupRepository;
    private final Logger logger = LoggerFactory.getLogger(RoleService.class);

    public RoleService(LookupRepository lookupRepository){
        this.lookupRepository = lookupRepository;
    }

    @Autowired
    private ModelMapper modelMapper;

//    @Transactional(transactionManager = "dmsTransactionManager")
//    public RoleDto createRole(LookupDto lookupDto) {
//    	lookupDto.setStatus(true);
//        Lookup savedRole = lookupRepository.save(modelMapper.map(lookupDto, Lookup.class));
//        return modelMapper.map(savedRole, RoleDto.class);
//    }

//    @Transactional(transactionManager = "dmsTransactionManager")
//    public RoleDto updateRole(RoleDto roleDto, Integer roleId) {
//        Role role = roleRepository.findById(roleId).orElseThrow(() -> new ResourceNotFoundException("Role", "Id", roleId));
//
//        role.setRoleName(roleDto.getRoleName());
//        role.setStatus(roleDto.getStatus());
//        Role updatedRole = roleRepository.save(role);
//        return modelMapper.map(updatedRole, RoleDto.class);
//    }

//    public List<RoleDto> getAllRoles(Integer pageNumber, Integer pageSize, String sortBy, String sortDir) {
//
//        Sort sort = (sortDir.equalsIgnoreCase("desc")) ? (Sort.by(sortBy).descending()) : (Sort.by(sortBy).ascending());
//        Pageable pageable = PageRequest.of(pageNumber, pageSize, sort);
//        Page<Role> pageRole = roleRepository.findAll(pageable);
//        List<Role> roles = pageRole.getContent();
//        return roles.stream().map(role -> modelMapper.map(role, RoleDto.class)).collect(Collectors.toList());
//    }
//
    public PageResponse<LookupDto> getAllRolesFromLookup(
           Integer pageNumber, Integer pageSize, String sortBy, String sortDir) {

        // Sorting
        Sort sort = sortDir.equalsIgnoreCase("desc") 
                ? Sort.by(sortBy).descending() 
                : Sort.by(sortBy).ascending();

        Pageable pageable = PageRequest.of(pageNumber, pageSize, sort);

        // Fetch only lookups filtered by setname
        Page<Lookup> pageRole = lookupRepository.findAllRolesFromLookup( pageable);

        // Convert entities to DTOs
        List<LookupDto> lookupDtos = pageRole.getContent()
                .stream()
                .map(role -> modelMapper.map(role, LookupDto.class))
                .collect(Collectors.toList());

        // Build page response
        PageResponse<LookupDto> pageResponse = new PageResponse<>();
        pageResponse.setContent(lookupDtos);
        pageResponse.setPageNumber(pageRole.getNumber());
        pageResponse.setPageSize(pageRole.getSize());
        pageResponse.setTotalElements(pageRole.getTotalElements());
        pageResponse.setTotalPages(pageRole.getTotalPages());
        pageResponse.setLastPage(pageRole.isLast());

        return pageResponse;
    }
    
 // Non-paginated version
    public List<LookupDto> getAllRolesFromLookupNoPagination() {
        List<Lookup> roles = lookupRepository.findAllRolesFromLookup();

        return roles.stream()
                .map(role -> {
                    LookupDto dto = new LookupDto();
                    dto.setLkId(role.getLkId());
                    dto.setStatus(role.getRecStatus());
                    dto.setLkLongname(role.getLongname()); // explicitly set longname
                    return dto;
                })
                .toList();
    }





//    public RoleDto getRoleByRoleId(Integer roleId) {
//        Role role = roleRepository.findByRoleId(roleId).orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", roleId));
//        return modelMapper.map(role, RoleDto.class);
//    }
//
//
//    public RoleDto getRoleByRoleName(String roleName) {
//        Role role = roleRepository.findByRoleName(roleName).orElseThrow(() -> new ResourceNotFoundException("Role", "Role Name", roleName));
//        return modelMapper.map(role, RoleDto.class);
//    }

//    public void disableRole(Integer roleId) {
//        roleRepository.findById(roleId)
//                .map(role -> {
//                    if (Boolean.FALSE.equals(role.getStatus())) {
//                        throw new ApiException("Role is already disabled");
//                    } else if (role.getRoleName().equalsIgnoreCase("ROLE_ADMIN")) {
//                        throw new ApiException("Admin role can not be disabled");
//                    }
//                    role.setStatus(false);
//                    return roleRepository.save(role);
//                })
//                .orElseThrow(() -> new ResourceNotFoundException("Role", " Id ", roleId));
//    }

//    public void enableRole(Integer roleId) {
//    	lookupRepository.findById(roleId)
//                .map(role -> {
//                    if (Boolean.TRUE.equals(role.getStatus())) {
//                        throw new ApiException("Role is already enabled");
//                    }
//                    role.setStatus(true);
//                    return roleRepository.save(role);
//                })
//                .orElseThrow(() -> new ResourceNotFoundException("Role", " Id ", roleId));
//    }

    //for authentication response, send only active role
    public Set<LookupDto> getActiveRoles(UserDetails userDetails) {
        logger.info("Fetching roles for user: {}", userDetails.getUsername());

        if (!(userDetails instanceof User)) {
            logger.warn("userDetails is not an instance of User, returning empty roles");
            return Collections.emptySet();
        }

        User user = (User) userDetails;

        if (user.getUserRoles() == null || user.getUserRoles().isEmpty()) {
            logger.info("No roles found for user={}", user.getUsername());
            return Collections.emptySet();
        }

        return user.getUserRoles().stream()
                .filter(ur -> ur.getUr_rec_status() != null && ur.getUr_rec_status() == 1
                           && ur.getRole() != null
                           && "ROLE".equalsIgnoreCase(ur.getRole().getSetname()))
                .map(ur -> {
                    Lookup lookup = ur.getRole();
                    logger.info("Active role={} for user={}", lookup.getLongname(), user.getUsername());
                    return modelMapper.map(lookup, LookupDto.class);
                })
                .collect(Collectors.toSet());
    }



}
