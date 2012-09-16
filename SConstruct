env = Environment(CCFLAGS='-O0 -ggdb -Wno-write-strings')
env.Append(CPPPATH = '.')
env.Append(LIBS = ['glut', 'GL', 'png'])
env.Append(CCFLAGS=' -DFLX_LINUX')

env.Program( target = 'nomadic', 
             source = ['main.cpp',
                       'core/fixed.cpp',
                       'core/list.cpp',
                       'core/geometry.cpp',
                       'engine/engine.cpp',
                       'engine/primitive.cpp',
                       'engine/text_primitive.cpp',
                       'engine/scenegraph.cpp',
                       'engine/scenenode.cpp',
                       'engine/texture.cpp',
                       'engine/nomadic.cpp',
                       'scheme/scheme.cpp',
                       'jellyfish/jellyfish_primitive.cpp',
                       'jellyfish/jellyfish.cpp'
                       ] )
