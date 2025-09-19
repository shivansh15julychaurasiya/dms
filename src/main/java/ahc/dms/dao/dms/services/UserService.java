package ahc.dms.dao.dms.services;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ahc.dms.dao.dms.entities.Lookup;
//import ahc.dms.dao.dms.entities.Role;
import ahc.dms.dao.dms.entities.User;
import ahc.dms.dao.dms.entities.UserRole;
import ahc.dms.dao.dms.repositories.LookupRepository;
//import ahc.dms.dao.dms.repositories.RoleRepository;
import ahc.dms.dao.dms.repositories.UserRepository;
import ahc.dms.dao.dms.repositories.UserRoleRepository;
import ahc.dms.exceptions.ApiException;
import ahc.dms.exceptions.DuplicateResourceException;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.dto.LookupDto;
import ahc.dms.payload.dto.RoleDto;
import ahc.dms.payload.dto.UserDto;
import ahc.dms.payload.dto.UserRoleDto;
import ahc.dms.payload.response.PageResponse;

@Service
public class UserService {

//	  ********************** JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
    @Autowired
    private UserRepository userRepository;
//    @Autowired
//    private RoleRepository roleRepository;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PasswordEncoder passwordEncoder;
    @Autowired
    private UserRoleRepository userRoleRepository;
    private LookupRepository lookupRepository;

//    @Transactional(transactionManager = "dmsTransactionManager")
//    public UserDto createUser(UserDto userDto) {
//        // username, email, phone
//        dataIntegrityValidation(userDto);
//
//        // Extract and validate a single RoleDto from userDto
//        Set<LookupDto> roles = userDto.getRoles();
//        if (roles == null || roles.isEmpty()) {
//            throw new ApiException("role_id must be provided");
//        }
//        if (roles.size() > 1) {
//            throw new ApiException("Only one role_id is allowed");
//        }
//        // Fetch the Role entity by ID
//        Lookup providedRole = fetchRoleOrThrow(roles.iterator().next().getLkId());
//        Lookup role=lookupRepository.findBy
//
//        // now save new user and the correct user-role mapping
//        userDto.setPassword(passwordEncoder.encode(userDto.getPassword()));
//        userDto.setStatus(true);
//        User newUser = userRepository.saveAndFlush(modelMapper.map(userDto, User.class));
//        UserRole userRole = userRoleRepository.saveAndFlush(new UserRole(newUser, providedRole, true));
//        RoleDto roleDto = modelMapper.map(userRole.getRole(), RoleDto.class);
//        // role should be active at user-role and roles level
//        roleDto.setStatus(userRole.getStatus() && providedRole.getRecStatus());
//
//        //preparing response
//        UserDto savedUserDto = modelMapper.map(newUser, UserDto.class);
//        savedUserDto.setRoles(new HashSet<>(Collections.singletonList(roleDto)));
//        savedUserDto.setUserRoles(null);
//
//        return savedUserDto;
//    }

//    @Transactional(transactionManager = "dmsTransactionManager")
//    public UserDto updateUser(UserDto userDto, Long userId) {
//
//        // Extract and validate a single RoleDto from userDto
//        Set<RoleDto> roles = userDto.getRoles();
//        if (roles == null || roles.isEmpty()) {
//            throw new ApiException("role_id must be provided");
//        }
//        if (roles.size() > 1) {
//            throw new ApiException("Only one role_id is allowed");
//        }
//        // Fetch the Role entity by ID
//        Lookup providedRole = fetchRoleOrThrow(roles.iterator().next().getRoleId());
//        // find and update user
//        User updatedUser = userRepository.findById(userId)
//                .map(user -> {
//                    if (Boolean.FALSE.equals(user.getStatus())) {
//                        throw new ApiException("User is deactivated. First activate the user.");
//                    }
//                    user.setUsername(userDto.getUsername());
//                    user.setName(userDto.getName());
//                    user.setEmail(userDto.getEmail());
//                    user.setAbout(userDto.getAbout());
//                    user.setPassword(passwordEncoder.encode(userDto.getPassword()));
//                    // Deactivate all roles
//                    user.getUserRoles().forEach(userRole -> userRole.setStatus(false));
//                    return userRepository.saveAndFlush(user);
//                })
//                .orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));
//
//        // if user-role exists then update status to true or create new active user-role
//        UserRole userRole = userRoleRepository.findByUserAndRole(updatedUser, providedRole)
//                .map(existingUserRole -> {
//                    // If exists, update status to true (since previous ones are disabled)
//                    existingUserRole.setStatus(true);
//                    return userRoleRepository.save(existingUserRole);
//                })
//                .orElseGet(() ->
//                        userRoleRepository.save(new UserRole(updatedUser, providedRole, true)));
//
//        //preparing response
//        UserDto updatedUserDto = modelMapper.map(updatedUser, UserDto.class);
//        RoleDto roleDto = modelMapper.map(userRole.getRole(), RoleDto.class);
//        // role should be active at user-role and roles level
//        roleDto.setStatus(userRole.getStatus() && userRole.getRole().getRecStatus());
//        updatedUserDto.setRoles(new HashSet<>(Set.of(roleDto)));
//        updatedUserDto.setUserRoles(null);
//        return updatedUserDto;
//    }
//
//    public UserDto getUserById(Long userId) {
//        User user = userRepository
//                .findById(userId)
//                .orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));
//
//        UserDto userDto = modelMapper.map(user, UserDto.class);
//        userDto.setUserRoles(null);
//        user.getActiveUserRole()
//                .ifPresentOrElse(role -> userDto.setRoles(Set.of(modelMapper.map(role, RoleDto.class))),
//                        () -> userDto.setRoles(new HashSet<>()));
//
//        return userDto;
//    }



        // Map active roles manually
        public UserDto getUserByUsername(String username) {
            // Fetch the user
            User user = userRepository.findByUsername(username)
                    .orElseThrow(() -> new ResourceNotFoundException("User", "Username", username));

            System.out.println("############################"+user);
            // Map basic fields manually
            UserDto userDto = new UserDto();
            userDto.setUserId(user.getUmId());
            userDto.setUsername(user.getUsername());
            userDto.setName(user.getName());
            userDto.setEmail(user.getEmail());
            userDto.setPhone(user.getUmMobile());
            userDto.setAbout(user.getAbout());
            userDto.setStatus(user.getStatus());
//            userDto.setRec_status(user.getRec_status());
            userDto.setCreatedAt(user.getCreatedAt());
            userDto.setUpdatedAt(user.getUpdatedAt());
          

            // Map active roles into LookupDto
            Set<LookupDto> roleDtos = user.getUserRoles().stream()
                    .filter(ur -> ur.getUr_rec_status() != null && ur.getUr_rec_status() == 1 && ur.getRole() != null)
                    .map(ur -> {
                        LookupDto lookupDto = new LookupDto();
                        lookupDto.setLkId(ur.getRole().getLkId());
                        lookupDto.setLkLongname(ur.getRole().getLongname());
                        return lookupDto;
                    })
                    .collect(Collectors.toSet());

            userDto.setRoles(roleDtos);

            return userDto;
        }


    public PageResponse<UserDto> getAllUsers(int pageNumber, int pageSize, String sortBy, String sortDir) {

        // 1️⃣ Prepare sorting and paging
        Sort sort = sortDir.equalsIgnoreCase("asc") ? Sort.by(sortBy).ascending() : Sort.by(sortBy).descending();
        Pageable pageable = PageRequest.of(pageNumber, pageSize, sort);

        // 2️⃣ Fetch page of users
        Page<User> userPage = userRepository.findAll(pageable);

        // 3️⃣ Map each User → UserDto
        List<UserDto> userDtos = userPage.getContent()
            .stream()
            .map(user -> {
                UserDto dto = new UserDto();
                dto.setUserId(user.getUmId());
                dto.setUsername(user.getUsername());
                dto.setStatus(user.getStatus());
                dto.setName(user.getName());
                dto.setFullName(user.getUm_fullname());
                dto.setEmail(user.getEmail());
                dto.setPhone(user.getPhone());
                dto.setAbout(user.getAbout());
                dto.setCreatedAt(user.getCreatedAt());
                dto.setUpdatedAt(user.getUpdatedAt());

                // --- Map userRoles (DTO representation) ---
                Set<UserRoleDto> userRoleDtos = user.getUserRoles()
                    .stream()
                    .filter(ur -> ur.getRole() != null) // avoid null Lookup
                    .map(ur -> {
                        UserRoleDto urDto = new UserRoleDto();
                        urDto.setUserId(user.getUmId());
                        urDto.setRoleId(ur.getRole().getLkId());
                        return urDto;
                    })
                    .collect(Collectors.toSet());
//                dto.setUserRoles(userRoleDtos);

                // --- Map roles (human-readable, from Lookup.longname) ---
                Set<LookupDto> roleDtos = user.getUserRoles()
                    .stream()
                    .filter(ur -> ur.getRole() != null && "DMS_ROLE".equals(ur.getRole().getSetname()))
                    .map(ur -> {
                        LookupDto lookup = new LookupDto();
                        lookup.setLkId(ur.getRole().getLkId());
                        lookup.setLkLongname(ur.getRole().getLongname()); // this gives human-readable role name
                        return lookup;
                    })
                    .collect(Collectors.toSet());
                dto.setRoles(roleDtos);

                return dto;
            })
            .collect(Collectors.toList());

        // 4️⃣ Build paginated response
        PageResponse<UserDto> response = new PageResponse<>();
        response.setContent(userDtos);
        response.setPageNumber(userPage.getNumber());
        response.setPageSize(userPage.getSize());
        response.setTotalElements(userPage.getTotalElements());
        response.setTotalPages(userPage.getTotalPages());
        response.setLastPage(userPage.isLast());

        return response;
    }


    @Transactional(transactionManager = "dmsTransactionManager")
    public void deactivateUser(String username) {
        userRepository.findByUsername(username)
            .map(user -> {
                if (user.getRec_status() == 2) {
                    throw new ApiException("User is already deactivated");
                }
                if (user.getVersion() == null) {
                    user.setVersion(0L); // temporary fix
                }
                user.setRec_status(2);
                return userRepository.save(user);
            })
            .orElseThrow(() -> new ResourceNotFoundException("User", "Username Id", username));
    }


    @Transactional(transactionManager = "dmsTransactionManager")
    public void activateUser(String username) {
        userRepository.findByUsername(username)
            .map(user -> {
            	 if (user.getRec_status() == 1) {
                     throw new ApiException("User is already activated");
                 }
               

                // TEMP FIX: initialize version if null
                if (user.getVersion() == null) {
                    user.setVersion(0L);
                }

                user.setRec_status(1);
                return userRepository.save(user);
            })
            .orElseThrow(() -> new ResourceNotFoundException("User", "Username Id", username));
    }


    public UserDto changePassword(String username, String password) {
        User user = userRepository.findByUsername(username).orElseThrow(() -> new ResourceNotFoundException("User", " Username", username));
        user.setPassword(passwordEncoder.encode(password));
        userRepository.save(user);
        return modelMapper.map(user, UserDto.class);
    }
//
//    //validation constraints
//    private void dataIntegrityValidation(UserDto userDto) {
//        // Check if email already exists
//        if (userRepository.existsByEmail(userDto.getEmail())) {
//            throw new DuplicateResourceException("Email", userDto.getEmail());
//        }
//        // Check if username already exists
//        if (userRepository.existsByUsername(userDto.getUsername())) {
//            throw new DuplicateResourceException("Username", userDto.getUsername());
//        }
//        // Check if phone already exists
//        if (userRepository.existsByPhone(userDto.getPhone())) {
//            throw new DuplicateResourceException("Phone number", userDto.getPhone());
//        }
//    }
//
//    private Lookup fetchRoleOrThrow(Long long1) {
//        return Optional.ofNullable(long1)
//                .map(id -> lookupRepository.findById(id)
//                        .map(lookup -> {
//                            if (Boolean.FALSE.equals(lookup.getRecStatus())) {
//                                throw new ApiException("Role is disabled");
//                            }
//                            return lookup;
//                        })
//                        .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", id)))
//                .orElseThrow(() -> new ApiException("Role Id cannot be null"));
//    }


}
