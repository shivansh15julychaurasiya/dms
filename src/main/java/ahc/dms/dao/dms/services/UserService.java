package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.Role;
import ahc.dms.dao.dms.entities.User;
import ahc.dms.dao.dms.entities.UserRole;
import ahc.dms.dao.dms.repositories.RoleRepository;
import ahc.dms.dao.dms.repositories.UserRoleRepository;
import ahc.dms.exceptions.ApiException;
import ahc.dms.exceptions.DuplicateResourceException;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.RoleDto;
import ahc.dms.payload.UserDto;
import ahc.dms.dao.dms.repositories.UserRepository;
import ahc.dms.payload.UserRoleDto;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
public class UserService {

    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoleRepository roleRepository;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PasswordEncoder passwordEncoder;
    @Autowired
    private UserRoleRepository userRoleRepository;

    @Transactional(transactionManager = "dmsTransactionManager")
    public UserDto createUser(UserDto userDto) {
        dataIntegrityValidation(userDto);

        // if user is present with provided login id
        userRepository.findByLoginId(userDto.getLoginId())
                .ifPresent(existingUser -> {
                    throw new DuplicateResourceException("Username (Login Id)", userDto.getLoginId());
                });

        // get the provided user-role from userDto
        UserRoleDto userRoleDto = Optional.ofNullable(userDto.getUserRoles())
                .filter(urDto -> urDto.size() == 1)
                .map(urDto -> urDto.iterator().next())
                .orElseThrow(() -> new ApiException(
                        userDto.getUserRoles() == null ? "User-role must be provide" :
                                userDto.getUserRoles().isEmpty() ? "At-least one user-role mapping must be provided" :
                                        "Not more than one user-role mapping is allowed"
                ));

        // find the provided role object
        Role providedRole = fetchRoleOrThrow(userRoleDto.getRoleId());

        // now save new user and the correct user-role mapping
        userDto.setPassword(passwordEncoder.encode(userDto.getPassword()));
        userDto.setStatus(true);
        User newUser = userRepository.saveAndFlush(modelMapper.map(userDto, User.class));
        UserRole userRole = userRoleRepository.saveAndFlush(new UserRole(newUser, providedRole, true));
        RoleDto roleDto = modelMapper.map(userRole.getRole(), RoleDto.class);
        roleDto.setStatus(userRole.getStatus());

        //preparing response
        UserDto newUserDto = modelMapper.map(newUser, UserDto.class);
        newUserDto.setRoles(new HashSet<>(Collections.singletonList(roleDto)));
        newUserDto.setUserRoles(null);

        return newUserDto;
        //return modelMapper.map(savedUser, UserDto.class);
    }

    @Transactional(transactionManager = "dmsTransactionManager")
    public UserDto updateUser(UserDto userDto, Long userId) {

        // get the provided user-role from userDto
        UserRoleDto userRoleDto = Optional.ofNullable(userDto.getUserRoles())
                .filter(urDto -> urDto.size() == 1)
                .map(urDto -> urDto.iterator().next())
                .orElseThrow(() -> new ApiException(
                        userDto.getUserRoles() == null ? "User-role must be provide" :
                                userDto.getUserRoles().isEmpty() ? "At-least one user-role mapping must be provided" :
                                        "Not more than one user-role mapping is allowed"
                ));

        // find the provided role object
        Role providedRole = fetchRoleOrThrow(userRoleDto.getRoleId());
        // find and update user
        User updatedUser = userRepository.findById(userId)
                .map(user -> {
                    if (Boolean.FALSE.equals(user.getStatus())) {
                        throw new ApiException("User is disabled");
                    }
                    user.setLoginId(userDto.getLoginId());
                    user.setName(userDto.getName());
                    user.setEmail(userDto.getEmail());
                    user.setAbout(userDto.getAbout());
                    user.setPassword(passwordEncoder.encode(userDto.getPassword()));
                    user.getUserRoles().forEach(userRole -> userRole.setStatus(false));
                    return userRepository.save(user);
                })
                .orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));

        // Deactivate all roles
        Set<UserRole> userRoles = userRoleRepository.findByUser(updatedUser);
        userRoles.forEach(ur -> ur.setStatus(false));
        userRoleRepository.saveAll(userRoles);

        // if user-role exists then update status to true or create new active user-role
        UserRole userRole = userRoleRepository.findByUserAndRole(updatedUser, providedRole)
                .map(existingUserRole -> {
                    // If exists, update status to true if it's not already true
                    existingUserRole.setStatus(true);
                    return userRoleRepository.save(existingUserRole);
                })
                .orElseGet(() -> userRoleRepository.save(new UserRole(updatedUser, providedRole, true)));

        //preparing response
        UserDto updatedUserDto = modelMapper.map(updatedUser, UserDto.class);
        RoleDto roleDto = modelMapper.map(userRole.getRole(), RoleDto.class);
        roleDto.setStatus(userRole.getStatus());
        updatedUserDto.setRoles(new HashSet<>(Collections.singletonList(roleDto)));
        updatedUserDto.setUserRoles(null);
        return updatedUserDto;
    }

    public UserDto getUserById(Long userId) {
        User user = userRepository.findById(userId).orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));
        return modelMapper.map(user, UserDto.class);
    }

    public UserDto getUserByLoginId(String loginId) {
        User user = userRepository.findByLoginId(loginId).orElseThrow(() -> new ResourceNotFoundException("User", "Login Id", loginId));
        return modelMapper.map(user, UserDto.class);
    }

    public List<UserDto> getAllUsers(Integer pageNumber, Integer pageSize, String sortBy, String sortDir) {

        Sort sort = (sortDir.equalsIgnoreCase("desc")) ? (Sort.by(sortBy).descending()) : (Sort.by(sortBy).ascending());
        Pageable pageable = PageRequest.of(pageNumber, pageSize, sort);
        Page<User> pageUser = userRepository.findAll(pageable);
        List<User> users = pageUser.getContent();

        List<UserDto> userDtoSet = new ArrayList<>();
        for (User user : users) {
            // convert user to dto
            UserDto userDto = modelMapper.map(user, UserDto.class);
            userDto.setUserRoles(null);

            // create role dto set
            Set<RoleDto> roleDtoSet = new HashSet<>();
            Role role = user.getActiveUserRole().orElseGet(null);
            if (role != null) {
                RoleDto roleDto = modelMapper.map(role, RoleDto.class);
                roleDtoSet.add(roleDto);
            }
            userDto.setRoles(roleDtoSet);
            userDtoSet.add(userDto);
        }

        return userDtoSet;
    }

    @Transactional(transactionManager = "dmsTransactionManager")
    public void deactivateUser(String loginId) {
        userRepository.findByLoginId(loginId)
                .map(user -> {
                    if (Boolean.FALSE.equals(user.getStatus())) {
                        throw new ApiException("User is already deactivated");
                    }
                    user.setStatus(false);
                    return userRepository.save(user);
                })
                .orElseThrow(() -> new ResourceNotFoundException("User", "Username Id", loginId));
    }

    @Transactional(transactionManager = "dmsTransactionManager")
    public void activateUser(String loginId) {
        userRepository.findByLoginId(loginId)
                .map(user -> {
                    if (Boolean.TRUE.equals(user.getStatus())) {
                        throw new ApiException("User is already active");
                    }
                    user.setStatus(true);
                    return userRepository.save(user);
                })
                .orElseThrow(() -> new ResourceNotFoundException("User", "Username Id", loginId));
    }

    public UserDto resetPassword(String loginId, String password) {
        User user = userRepository.findByLoginId(loginId).orElseThrow(() -> new ResourceNotFoundException("User", " Login Id ", loginId));
        user.setPassword(passwordEncoder.encode(password));
        userRepository.save(user);
        return modelMapper.map(user, UserDto.class);
    }

    //validation constraints
    private void dataIntegrityValidation(UserDto userDto) {
        // Check if email already exists
        if (userRepository.existsByEmail(userDto.getEmail())) {
            throw new DuplicateResourceException("Email", userDto.getEmail());
        }
        // Check if loginId already exists
        if (userRepository.existsByLoginId(userDto.getLoginId())) {
            throw new DuplicateResourceException("Login ID", userDto.getLoginId());
        }
        // Check if phone already exists
        if (userRepository.existsByPhone(userDto.getPhone())) {
            throw new DuplicateResourceException("Phone number", userDto.getPhone());
        }
    }

    private User fetchUserOrThrow(Long userId) {
        return Optional.ofNullable(userId)
                .map(id -> userRepository.findById(id)
                        .map(user -> {
                            if (Boolean.FALSE.equals(user.getStatus())) {
                                throw new ApiException("User is disabled");
                            }
                            return user;
                        })
                        .orElseThrow(() -> new ResourceNotFoundException("User", "User Id", id)))
                .orElseThrow(() -> new ApiException("User Id cannot be null"));
    }

    private Role fetchRoleOrThrow(Integer roleId) {
        return Optional.ofNullable(roleId)
                .map(id -> roleRepository.findByRoleId(id)
                        .map(role -> {
                            if (Boolean.FALSE.equals(role.getStatus())) {
                                throw new ApiException("Role is disabled");
                            }
                            return role;
                        })
                        .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", id)))
                .orElseThrow(() -> new ApiException("Role Id cannot be null"));
    }

}
