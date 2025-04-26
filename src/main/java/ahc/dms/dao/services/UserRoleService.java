package ahc.dms.dao.services;

import ahc.dms.dao.entities.Role;
import ahc.dms.dao.entities.User;
import ahc.dms.dao.entities.UserRole;
import ahc.dms.dao.respositories.RoleRepository;
import ahc.dms.dao.respositories.UserRepository;
import ahc.dms.dao.respositories.UserRoleRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.RoleDto;
import ahc.dms.payload.UserDto;
import ahc.dms.payload.UserRoleDto;
import jakarta.validation.Valid;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

    @Transactional
    public UserRoleDto assignRole(UserRoleDto userRoleDto) {
        User user = userRepository
                .findById(userRoleDto.getUserId())
                .orElseThrow(() -> new ResourceNotFoundException("User", "Role Id", userRoleDto.getUserId()));
        Role role = roleRepository
                .findByRoleId(userRoleDto.getRoleId())
                .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", userRoleDto.getRoleId()));
        UserRole userRole = userRoleRepository.save(new UserRole(user, role, true));

        return modelMapper.map(userRole, UserRoleDto.class);


    }
}
