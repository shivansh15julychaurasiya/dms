package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.ObjectMaster;
import ahc.dms.dao.dms.repositories.ObjectMasterRepository;
import ahc.dms.dao.dms.repositories.ObjectRoleRepository;
import ahc.dms.dao.dms.repositories.RoleRepository;
import ahc.dms.exceptions.ApiException;
import ahc.dms.exceptions.DuplicateResourceException;
import ahc.dms.payload.dto.ObjectMasterDto;
import jakarta.validation.Valid;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ObjectMasterService {

    @Autowired
    private ObjectMasterRepository omRepository;
    @Autowired
    private ModelMapper modelMapper;
    private final Logger logger = LoggerFactory.getLogger(ObjectMasterService.class);


    public ObjectMasterDto createObjectMaster(@Valid ObjectMasterDto omDto) {


        ObjectMaster om = omRepository
                .findByRequestUriAndRequestMethod(omDto.getRequestUri(), omDto.getRequestMethod())
                .map(objectMaster -> {
                    if (Boolean.FALSE.equals(objectMaster.getStatus())) {
                        throw new ApiException("Object (url) is disabled");
                    } else if (Boolean.TRUE.equals(objectMaster.getStatus())) {
                        throw new DuplicateResourceException(objectMaster.getRequestUri(), objectMaster.getRequestMethod());
                    }
                    return objectMaster;
                })
                .orElseGet(() -> {
                    ObjectMaster newOm = modelMapper.map(omDto, ObjectMaster.class);
                    newOm.setStatus(Boolean.TRUE);
                    return omRepository.save(newOm);
                });

        return modelMapper.map(om, ObjectMasterDto.class);

    }

}
