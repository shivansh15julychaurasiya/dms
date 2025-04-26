package ahc.dms.dao.services;

import ahc.dms.dao.entities.Role;
import ahc.dms.dao.respositories.RoleRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.RoleDto;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RoleService {

    private final RoleRepository roleRepository;

    public RoleService(RoleRepository roleRepository){
        this.roleRepository = roleRepository;
    }

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public RoleDto createRole(RoleDto roleDto) {
        Role savedRole = roleRepository.save(modelMapper.map(roleDto, Role.class));
        return modelMapper.map(savedRole, RoleDto.class);
    }

    @Transactional
    public RoleDto updateRole(RoleDto roleDto, Integer roleId) {
        Role role = roleRepository.findById(roleId).orElseThrow(() -> new ResourceNotFoundException("Role", "Id", roleId));

        role.setRoleName(roleDto.getRoleName());
        role.setStatus(roleDto.isStatus());
        Role updatedRole = roleRepository.save(role);
        return modelMapper.map(updatedRole, RoleDto.class);
    }

    public List<RoleDto> getAllRoles() {
        List<Role> roles = roleRepository.findAll();
        return roles.stream().map(role -> modelMapper.map(role, RoleDto.class)).collect(Collectors.toList());
    }


    public RoleDto getRoleByRoleId(Integer roleId) {
        Role role = roleRepository.findByRoleId(roleId).orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", roleId));
        return modelMapper.map(role, RoleDto.class);
    }


    public RoleDto getRoleByRoleName(String roleName) {
        Role role = roleRepository.findByRoleName(roleName).orElseThrow(() -> new ResourceNotFoundException("Role", "Role Name", roleName));
        return modelMapper.map(role, RoleDto.class);
    }

    public void deleteRole(Integer roleId) {
        Role role = roleRepository.findById(roleId).orElseThrow(() -> new ResourceNotFoundException("Role", " Id ", roleId));
        roleRepository.delete(role);
    }
}
