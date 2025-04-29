package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.Role;
import ahc.dms.dao.dms.entities.User;
import ahc.dms.dao.dms.entities.UserRole;
import ahc.dms.dao.dms.repositories.RoleRepository;
import ahc.dms.dao.dms.repositories.UserRepository;
import ahc.dms.dao.dms.repositories.UserRoleRepository;
import ahc.dms.exceptions.ApiException;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.RoleDto;
import ahc.dms.payload.UserDto;
import ahc.dms.payload.UserRoleDto;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class UserRoleService {

    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoleRepository roleRepository;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private UserRoleRepository userRoleRepository;

    @Transactional(transactionManager = "dmsTransactionManager")
    public UserDto assignRole(UserRoleDto userRoleDto) {

        //check for existing user
        User existingUser = Optional.ofNullable(userRoleDto.getUserId())
                .map(userId -> userRepository
                        .findById(userId)
                        .orElseThrow(() -> new ResourceNotFoundException("User", "User Id", userRoleDto.getUserId())))
                .orElseThrow(() -> new ApiException("User Id cannot be null"));
        //check for existing role
        Role existingRole = Optional.ofNullable(userRoleDto.getRoleId())
                .map(roleId -> roleRepository.findByRoleId(userRoleDto.getRoleId())
                        .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", userRoleDto.getRoleId())))
                .orElseThrow(() -> new ApiException("Role Id cannot be null"));

        //check for existing role-mapping
        Optional<UserRole> existingUserRole = userRoleRepository.findByUserAndRole(existingUser, existingRole);
        if (existingUserRole.isPresent() && existingUserRole.get().getStatus()) {
            // if an active user-role mapping already exists
            throw new ApiException("Mapping already exists");
        } else if (existingUserRole.isPresent()){
            // deactivate all other user-role mapping
            existingUser.getUserRoles().forEach(userRole -> userRole.setStatus(false));
            userRepository.saveAndFlush(existingUser);
            // if the user-role mapping is stale, then active it
            existingUserRole.get().setStatus(true);
            userRoleRepository.saveAndFlush(existingUserRole.get());
        } else {
            //first deactivate all the previous user-role mapping
            existingUser.getUserRoles().forEach(userRole -> userRole.setStatus(false));
            userRepository.saveAndFlush(existingUser);
            //create a new active user-role mapping
            UserRole newUserRole = new UserRole(existingUser, existingRole, true);
            userRoleRepository.save(newUserRole);
        }

        // Refresh the user entity in persistence context
        existingUser = userRepository.findById(existingUser.getUserId()).orElseThrow();

        // Get updated roles directly from the managed user entity
        Set<Role> roles = existingUser.getUserRoles().stream()
                .filter(UserRole::getStatus)
                .map(UserRole::getRole)
                .collect(Collectors.toSet());

        // Prepare response
        UserDto theUserDto = modelMapper.map(existingUser, UserDto.class);
        theUserDto.setRoles(roles.stream()
                .map(eachrole -> modelMapper.map(eachrole, RoleDto.class))
                .collect(Collectors.toSet()));
        theUserDto.setUserRoles(null);
        return theUserDto;
    }

    public UserDto deassignRole(UserRoleDto userRoleDto) {
        //check for existing user
        User existingUser = Optional.ofNullable(userRoleDto.getUserId())
                .map(userId -> userRepository
                        .findById(userId)
                        .orElseThrow(() -> new ResourceNotFoundException("User", "User Id", userRoleDto.getUserId())))
                .orElseThrow(() -> new ApiException("User Id cannot be null"));
        //check for existing role
        Role existingRole = Optional.ofNullable(userRoleDto.getRoleId())
                .map(roleId -> roleRepository.findByRoleId(userRoleDto.getRoleId())
                        .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", userRoleDto.getRoleId())))
                .orElseThrow(() -> new ApiException("Role Id cannot be null"));
        //check for existing role-mapping
        Optional<UserRole> existingUserRole = userRoleRepository.findByUserAndRole(existingUser, existingRole);
        if (existingUserRole.isPresent()) {
            if (existingUserRole.get().getStatus()) {
                // deactivate active user-role
                existingUserRole.get().setStatus(false);
                userRoleRepository.save(existingUserRole.get());
            } else {
                // if user-role is already disabled
                throw new ApiException("Role already de-assigned");
            }
        } else {
            // no user-role mapping found (active or stale)
            throw new ApiException("User has never been assigned given Role");
        }

        // Refresh the user entity in persistence context
        existingUser = userRepository.findById(existingUser.getUserId()).orElseThrow();

        // Get updated roles directly from the managed user entity
        Set<Role> roles = existingUser.getUserRoles().stream()
                .filter(UserRole::getStatus)
                .map(UserRole::getRole)
                .collect(Collectors.toSet());

        // Prepare response
        UserDto theUserDto = modelMapper.map(existingUser, UserDto.class);
        theUserDto.setRoles(roles.stream()
                .map(eachrole -> modelMapper.map(eachrole, RoleDto.class))
                .collect(Collectors.toSet()));
        theUserDto.setUserRoles(null);
        return theUserDto;
    }



}
