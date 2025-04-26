package ahc.dms.dao.services;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.entities.Role;
import ahc.dms.dao.entities.User;
import ahc.dms.dao.entities.UserRole;
import ahc.dms.dao.respositories.RoleRepository;
import ahc.dms.dao.respositories.UserRoleRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.RoleDto;
import ahc.dms.payload.UserDto;
import ahc.dms.dao.respositories.UserRepository;
import ahc.dms.payload.UserRoleDto;
import jakarta.validation.Valid;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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

//    @Transactional
//    public UserDto registerNewUser(UserDto userDto) {
//
//        System.out.println("user dto");
//        System.out.println(userDto.toString());
//        userDto.setPassword(passwordEncoder.encode(userDto.getPassword()));
//        User user = modelMapper.map(userDto, User.class);
//
//        Role role = roleRepository.findById(AppConstants.NORMAL_USER).get();
//        user.getRoles().add(role);
//
//        User newUser = userRepository.save(user);
//        return modelMapper.map(newUser, UserDto.class);
//    }

    @Transactional
    public UserDto createUser(UserDto userDto) {
        userDto.setPassword(passwordEncoder.encode(userDto.getPassword()));

        //first save the user to get userid
        User user = modelMapper.map(userDto, User.class);
        User savedUser = userRepository.save(user);
        //preparing to map roles since modelmapper is not able to map nested classes properly
        Set<RoleDto> roleDtos = new HashSet<>();
        //if user-roles are provided the add them
        if (userDto.getUserRoles() != null && !userDto.getUserRoles().isEmpty()){
            Set<UserRole> userRoles = new HashSet<>();
            for (UserRoleDto userRoleDto : userDto.getUserRoles()) {
                Role role = roleRepository
                        .findByRoleId(userRoleDto.getRoleId())
                        .orElseThrow(() -> new ResourceNotFoundException("Role", "Role Id", userRoleDto.getRoleId()));
                // create new userrole
                UserRole userRole = new UserRole(savedUser, role, true);
                userRoles.add(userRole);
                //create role dtos for response
                RoleDto roleDto = modelMapper.map(role, RoleDto.class);
                roleDtos.add(roleDto);
            }
            userRoleRepository.saveAll(userRoles);
        }
        //preparing response
        UserDto savedUserDto = modelMapper.map(savedUser, UserDto.class);
        savedUserDto.setRoles(roleDtos);
        savedUserDto.setUserRoles(null);

        return savedUserDto;
        //return modelMapper.map(savedUser, UserDto.class);
    }

    @Transactional
    public UserDto updateUser(UserDto userDto, Integer userId) {
        User user = userRepository.findById(userId).orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));

        user.setLoginId(userDto.getLoginId());
        user.setName(userDto.getName());
        user.setEmail(userDto.getEmail());
        user.setAbout(userDto.getAbout());
        user.setPassword(passwordEncoder.encode(userDto.getPassword()));

        User updatedUser = userRepository.save(user);
        return modelMapper.map(updatedUser, UserDto.class);
    }

    public UserDto getUserById(Integer userId) {
        User user = userRepository.findById(userId).orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));
        return modelMapper.map(user, UserDto.class);
    }

    public UserDto getUserByLoginId(String loginId) {
        User user = userRepository.findByLoginId(loginId).orElseThrow(() -> new ResourceNotFoundException("User", "Login Id", loginId));
        return modelMapper.map(user, UserDto.class);
    }

    public List<UserDto> getAllUsers() {
        List<User> users = userRepository.findAll();
        return users.stream().map(user -> modelMapper.map(user, UserDto.class)).collect(Collectors.toList());
    }

    @Transactional
    public void deleteUser(Integer userId) {
        User user = userRepository.findById(userId).orElseThrow(() -> new ResourceNotFoundException("User", " Id ", userId));
        userRepository.delete(user);
    }

    public UserDto resetPassword(String loginId, String password) {
        User user = userRepository.findByLoginId(loginId).orElseThrow(() -> new ResourceNotFoundException("User", " Login Id ", loginId));
        user.setPassword(passwordEncoder.encode(password));
        userRepository.save(user);
        return modelMapper.map(user, UserDto.class);
    }
}
