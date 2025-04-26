package ahc.dms;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.entities.Role;
import ahc.dms.dao.entities.UserRole;
import ahc.dms.dao.respositories.RoleRepository;
import ahc.dms.payload.UserRoleDto;
import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.modelmapper.config.Configuration;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;

@SpringBootApplication
//public class DmsApplication extends SpringBootServletInitializer implements CommandLineRunner{
public class DmsApplication implements CommandLineRunner {

    @Autowired
    private PasswordEncoder passwordEncoder;
    @Autowired
    private RoleRepository roleRepository;

    public static void main(String[] args) {
        SpringApplication.run(DmsApplication.class, args);
    }

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();

		// Configure global settings
		modelMapper.getConfiguration()
				.setMatchingStrategy(MatchingStrategies.STANDARD) // Use STRICT for exact matching
				.setAmbiguityIgnored(true) // ignore ambiguous matches
				.setFieldAccessLevel(Configuration.AccessLevel.PRIVATE);

		// Explicit UserRole to UserRoleDto mapping
		modelMapper.typeMap(UserRole.class, UserRoleDto.class)
				.addMappings(mapper -> {
					// Explicitly define all mappings
					//mapper.map(src -> src.getUrId(), UserRoleDto::setUrId);
					mapper.map(src -> src.getUser().getUserId(), UserRoleDto::setUserId);
					mapper.map(src -> src.getRole().getRoleId(), UserRoleDto::setRoleId);
					//mapper.map(src -> src.isStatus(), UserRoleDto::setStatus);
				});

		// Validate the configuration
		modelMapper.validate();
		return  modelMapper;
    }

    @Override
    public void run(String... args) {
        System.out.println(this.passwordEncoder.encode("1234"));
        try {
            Role adminRole = new Role();
            adminRole.setRoleId(AppConstants.ADMIN_USER);
            adminRole.setRoleName("ROLE_ADMIN");

            Role userRole = new Role();
            userRole.setRoleId(AppConstants.NORMAL_USER);
            userRole.setRoleName("ROLE_USER");

            Role ecourtRole = new Role();
            ecourtRole.setRoleId(AppConstants.ECOURT_USER);
            ecourtRole.setRoleName("ROLE_ECOURT");

            List<Role> roles = List.of(adminRole, userRole, ecourtRole);
            List<Role> savedRoles = roleRepository.saveAll(roles);
            savedRoles.forEach(r -> System.out.println(r.getRoleName()));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
