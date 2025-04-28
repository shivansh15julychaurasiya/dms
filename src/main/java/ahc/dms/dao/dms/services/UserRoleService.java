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

        if (userRoleDto.getUserId() == null)
            throw new ApiException("User Id required");
        if (userRoleDto.getRoleId() == null)
            throw new ApiException("Role Id required");

        //check for existing user
        User user = userRepository
                .findById(userRoleDto.getUserId())
                .orElseThrow(() -> new ResourceNotFoundException("User", "Role Id", userRoleDto.getUserId()));
        //check for existing role
        Role role = roleRepository
                .findByRoleId(userRoleDto.getRoleId())
                .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", userRoleDto.getRoleId()));
        //check for existing role-mapping
        Optional<UserRole> existingRole = userRoleRepository.findByUserAndRole(user, role);
        if (existingRole.isPresent()) {
            if (existingRole.get().getStatus()) {
                throw new ApiException("Mapping already exists");
            } else {
                existingRole.get().setStatus(true);
                // No need to explicitly save - will be flushed at transaction end
                userRoleRepository.save(existingRole.get());
            }
        } else {
            UserRole newUserRole = new UserRole(user, role, true);
            user.getUserRoles().add(newUserRole); // Maintain bidirectional relationship
            //saving new role
            userRoleRepository.save(newUserRole);
        }

        // Refresh the user entity in persistence context
        user = userRepository.findById(user.getUserId()).orElseThrow();

        // Get updated roles directly from the managed user entity
        Set<Role> roles = user.getUserRoles().stream()
                .filter(UserRole::getStatus)
                .map(UserRole::getRole)
                .collect(Collectors.toSet());

        // Prepare response
        UserDto theUserDto = modelMapper.map(user, UserDto.class);
        theUserDto.setRoles(roles.stream()
                .map(eachrole -> modelMapper.map(eachrole, RoleDto.class))
                .collect(Collectors.toSet()));
        theUserDto.setUserRoles(null);
        return theUserDto;
    }

    public UserDto deassignRole(UserRoleDto userRoleDto) {
        if (userRoleDto.getUserId() == null)
            throw new ApiException("User Id required");
        if (userRoleDto.getRoleId() == null)
            throw new ApiException("Role Id required");
        //check for existing user
        User user = userRepository
                .findById(userRoleDto.getUserId())
                .orElseThrow(() -> new ResourceNotFoundException("User", "Role Id", userRoleDto.getUserId()));
        //check for existing role
        Role role = roleRepository
                .findByRoleId(userRoleDto.getRoleId())
                .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", userRoleDto.getRoleId()));
        //check for existing role-mapping
        Optional<UserRole> existingRole = userRoleRepository.findByUserAndRole(user, role);
        if (existingRole.isPresent()) {
            if (existingRole.get().getStatus()) {
                existingRole.get().setStatus(false);
                userRoleRepository.save(existingRole.get());
            } else {
                throw new ApiException("Role already de-assigned");
            }
        } else {
            throw new ApiException("User has never been assigned given Role");
        }

        // Refresh the user entity in persistence context
        user = userRepository.findById(user.getUserId()).orElseThrow();

        // Get updated roles directly from the managed user entity
        Set<Role> roles = user.getUserRoles().stream()
                .filter(UserRole::getStatus)
                .map(UserRole::getRole)
                .collect(Collectors.toSet());

        // Prepare response
        UserDto theUserDto = modelMapper.map(user, UserDto.class);
        theUserDto.setRoles(roles.stream()
                .map(eachrole -> modelMapper.map(eachrole, RoleDto.class))
                .collect(Collectors.toSet()));
        theUserDto.setUserRoles(null);
        return theUserDto;
    }
}
