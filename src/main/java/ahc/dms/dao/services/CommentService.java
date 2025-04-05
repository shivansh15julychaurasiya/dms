package ahc.dms.dao.services;

import ahc.dms.dao.entities.Comment;
import ahc.dms.dao.entities.Post;
import ahc.dms.dao.entities.User;
import ahc.dms.dao._payloads.CommentDto;
import ahc.dms.dao.respositories.CommentRepository;
import ahc.dms.dao.respositories.PostRepository;
import ahc.dms.dao.respositories.UserRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CommentService {

    @Autowired
    private CommentRepository commentRepository;
    @Autowired
    private PostRepository postRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private ModelMapper modelMapper;

    public CommentDto createComment(CommentDto commentDto, Integer userId, Integer postId) {

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));
        Post post = postRepository.findById(postId)
                .orElseThrow(() -> new ResourceNotFoundException("Post", "Id", postId));

        Comment comment = modelMapper.map(commentDto, Comment.class);
        comment.setUser(user);
        comment.setPost(post);
        Comment savedComment = commentRepository.save(comment);

        return modelMapper.map(savedComment, CommentDto.class);
    }

    public void deleteComment(Integer commentId) {
        Comment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new ResourceNotFoundException("Comment", "Id", commentId));
        commentRepository.delete(comment);
    }

}
