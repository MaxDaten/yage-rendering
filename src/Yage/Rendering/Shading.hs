

type AttribDef = ident -> vad -> m a
type UniformDefs = self -> globals -> m a

ShaderDef = ShaderDef 
    { sd'shaderProgram :: ShaderProgram
    , sd'attrDefs    :: AttribDef
    , sd'uniDefs   :: UniformDefs
    }

sampleDef = ShaderDef
    { sd'shaderProgram = ShaderProgram ...
    , sd'attrDefs = do -- compiled mesh input?
        "in_vert_pos" `enableAttr` vad
        "in_vert_col" `enableAttr` vad
    , sd'uniDefs  = \r s -> do
        "model_matrix" `setUni` model_matrix r -- more generic?!
        "uni_global_time" `setUni` sceneTime s
    }