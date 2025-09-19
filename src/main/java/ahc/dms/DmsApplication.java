package ahc.dms;

import ahc.dms.dao.dms.entities.*;
import ahc.dms.dao.dms.repositories.*;
import ahc.dms.payload.dto.UserRoleDto;
import org.modelmapper.ModelMapper;
import org.modelmapper.config.Configuration;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.transaction.annotation.Transactional;

@SpringBootApplication
@EnableJpaAuditing(auditorAwareRef = "auditorAwareImpl")
public class DmsApplication extends SpringBootServletInitializer implements CommandLineRunner {
	
//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************

    @Autowired
    private PasswordEncoder passwordEncoder;
//    @Autowired
//    private RoleRepository roleRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserRoleRepository userRoleRepository;

    public static void main(String[] args) {
        SpringApplication.run(DmsApplication.class, args);
    }

    // ModelMapper bean
    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();

        // Configure global settings
        modelMapper.getConfiguration()
                .setMatchingStrategy(MatchingStrategies.STANDARD) // Use STRICT for exact matching
                .setAmbiguityIgnored(true) // ignore ambiguous matches
                .setFieldAccessLevel(Configuration.AccessLevel.PRIVATE);

        // Explicit UserRole to UserRoleDto mapping
//        modelMapper.typeMap(UserRole.class, UserRoleDto.class)
//                .addMappings(mapper -> {
                    // Explicitly define all mappings
                    //mapper.map(src -> src.getUrId(), UserRoleDto::setUrId);
//                    mapper.map(src -> src.getUser().getUserId(), UserRoleDto::setUserId);
//                    mapper.map(src -> src.getRole().getRoleId(), UserRoleDto::setRoleId);
                    //mapper.map(src -> src.isStatus(), UserRoleDto::setStatus);
//                });

        // Validate the configuration
        modelMapper.validate();
        return modelMapper;
    }



//    @Override
//    @Transactional
//    public void run(String... args) {
//        try {
//            // Create roles if not exists
//            Role adminRole = roleRepository.findByRoleName("ROLE_ADMIN")
//                    .orElseGet(() -> roleRepository.save(new Role("ROLE_ADMIN", true)));
//
//            Role userRole = roleRepository.findByRoleName("ROLE_USER")
//                    .orElseGet(() -> roleRepository.save(new Role("ROLE_USER", true)));
//
//            Role ecourtRole = roleRepository.findByRoleName("ROLE_ECOURT")
//                    .orElseGet(() -> roleRepository.save(new Role("ROLE_ECOURT", true)));
//
//            // Create default user if not exists
//            User firstUser = userRepository.findByUsername("11448")
//                    .orElseGet(() -> {
//                        User newUser = new User();
//                        newUser.setName("Vijay Chaurasiya");
//                        newUser.setUsername("11448");
//                        newUser.setEmail("vijaychaurasiya@gmail.com");
//                        newUser.setAbout("admin");
//                        newUser.setPhone("9721308763");
//                        newUser.setPassword(passwordEncoder.encode("1234"));
//                        newUser.setCr_by(1L); // system user
//                        newUser.setRec_status(1); // active by default
//                        return userRepository.saveAndFlush(newUser);
//                    });
//
//            // Assign admin role to user
//            if (!userRoleRepository.existsByUserAndRole(firstUser, adminRole)) {
//                userRoleRepository.save(new UserRole(firstUser, adminRole, true));
//            }
//
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }

    // Optional: inspect security filter chain
    @Bean
    public ApplicationListener<ApplicationReadyEvent> filterChainInspector() {
        return event -> {
            FilterChainProxy filterChainProxy = event.getApplicationContext()
                    .getBean(FilterChainProxy.class);
            System.out.println("\n===== SECURITY FILTER CHAIN ORDER =====");
            filterChainProxy.getFilterChains().forEach(chain -> {
                System.out.println("\nDefault filters:");
                chain.getFilters().forEach(filter ->
                        System.out.println("- " + filter.getClass().getSimpleName()));
            });
            System.out.println("===== END FILTER CHAIN =====");
        };
    }

	@Override
	public void run(String... args) throws Exception {
		// TODO Auto-generated method stub
		
	}
}
